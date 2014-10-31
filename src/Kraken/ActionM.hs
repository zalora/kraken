{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveGeneric,
             FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, ScopedTypeVariables #-}

module Kraken.ActionM (
    TargetName(..),
    showError,

    Error(..),
    ActionM,
    TargetM,
    MonitorM,
    cancel,
    runActionM,

    withTargetName,

    logError,
    IsolateResult(..),
    isolate,
    catch,
    mapExceptions,

    triggerTarget,
    outOfDate,
    discardMonitorInput,
    bracketWithMonitor,
    memoizeMonitorInput,
  ) where


import           Control.Applicative
import           Control.Arrow              ((>>>))
import qualified Control.Exception          as E
import           Control.Exception.Enclosed
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State        (StateT, get, put, runStateT)
import           Control.Monad.Trans.Either
import           Data.Monoid
import           Data.String
import           Data.Typeable
import           GHC.Generics

import           Kraken.Util


newtype TargetName = TargetName String
  deriving (Eq, Ord, IsString, Monoid, Generic)

instance Show TargetName where
    show (TargetName n) = n


data Error = Error {
    errorTarget :: Maybe TargetName,
    errorMessage :: String
  }
    deriving (Eq, Ord, Show)

showError :: Error -> String
showError (Error mTarget message) = unlines $
    ((maybe "<no target>" show mTarget) ++ ":") :
    (fmap ("    " ++) (lines message))


data ShortCut monitorInput
  = ErrorShortCut Error
  | OutDated Error (Maybe monitorInput)
    deriving (Functor)

type State = [Error]


-- | Our monad to run monitors and targets.
data ActionM monitorInput a = ActionM {
    getActionM :: EitherT (ShortCut monitorInput) (StateT State (ReaderT (Maybe TargetName) IO)) a
  }
    deriving (Functor)

-- | Monad for actions for targets.
type TargetM a = ActionM () a
-- | Monad for actions in monitors.
type MonitorM monitorInput a = ActionM monitorInput a

instance Applicative (ActionM monitorInput) where
    pure = ActionM . pure
    ActionM f <*> ActionM x = ActionM (f <*> x)

instance Monad (ActionM monitorInput) where
    ActionM a >>= f = ActionM $ do
        x <- a
        let (ActionM b) = f x
        b
    return = ActionM . return
    fail msg = do
        logMessageLn "ActionM.fail is discouraged: please, use cancel"
        cancel msg

cancel :: String -> ActionM monitorInput a
cancel msg = ActionM $ do
    target <- ask
    let error = Error target msg
    logMessage $ showError error
    left $ ErrorShortCut error

instance MonadIO (ActionM monitorInput) where
    liftIO = ActionM . liftIO

unwrap :: State -> Maybe TargetName -> ActionM monitorInput a -> IO (Either (ShortCut monitorInput) a, State)
unwrap state currentTarget (ActionM action) =
    runReaderT (runStateT (runEitherT action) state) currentTarget


runActionM :: ActionM monitorInput a -> IO (Either [Error] a)
runActionM action = do
    (result, state) <- unwrap [] Nothing action
    return $ case (result, state) of
        (Right x, []) -> Right x
        (Right _, errors) -> Left errors
        (Left (ErrorShortCut error), errors) -> Left (errors ++ [error])
        (Left (OutDated error _), errors) ->
            -- OutDated treated like an error, because it's not being executed
            -- inside bracketWithMonitor.
            Left (errors ++ [error])

-- | Sets the TargetName that will be included in the error messages that
-- can be raised by 'cancel' and 'outOfDate'.
withTargetName :: TargetName -> ActionM monitorInput a -> ActionM monitorInput a
withTargetName name (ActionM action) =
    mapExceptions (ExceptionWithTargetName name) $
        ActionM $ local (const $ Just name) action

-- | Helper type to add the current target name to thrown exceptions.
data ExceptionWithTargetName = ExceptionWithTargetName TargetName E.SomeException
    deriving (Typeable)

instance Show ExceptionWithTargetName where
    show (ExceptionWithTargetName _ e) = show e

instance E.Exception ExceptionWithTargetName

-- | Extracts the added target name from an exception, if any.
extractTargetName :: E.SomeException -> Maybe TargetName
extractTargetName (E.SomeException e) = case cast e of
    Just (ExceptionWithTargetName targetName _) -> Just targetName
    Nothing -> Nothing


-- | Issues an error. The error will
-- - be written to stderr,
-- - be included in the error summary that is output before the process ends and
-- - cause the process to exit with a non-zero exitcode.
-- However, it will not shortcut the execution of the monadic action.
logError :: String -> ActionM x ()
logError msg = ActionM $ do
    currentTarget <- ask
    let error = Error currentTarget msg
    logMessage $ showError error
    get >>= ((++ [error]) >>> put)


data IsolateResult
  = IsolateSuccess
  | IsolateFailure
 deriving (Eq, Ord, Show, Read, Typeable)

-- | Runs the given action and always succeeds. Both Lefts and Exceptions are
-- being written to the error state. Returns whether something went wrong.
isolate :: ActionM monitorInput () -> ActionM monitorInput IsolateResult
isolate action = do
    currentTarget <- ActionM ask
    result <- liftIO $ catchAny
        (runActionM ((maybe id withTargetName currentTarget) action))
        (\ e -> do
            let error = Error (extractTargetName e) (show e)
            logMessage $ showError error
            return $ Left [error])
    case result of
        Right () -> return IsolateSuccess
        Left errs -> ActionM $ do
            state <- get
            put (state ++ errs)
            return IsolateFailure

mapExceptions :: (E.Exception a, E.Exception b) => (a -> b) -> ActionM monitorInput o -> ActionM monitorInput o
mapExceptions f action = catch action (liftIO . E.throwIO . f)

catch :: E.Exception e => ActionM monitorInput a -> (e -> ActionM monitorInput a) -> ActionM monitorInput a
catch action handler = ActionM $ do
    state <- get
    currentTarget <- ask
    (result, nextState) <- liftIO $ E.catch
        (unwrap state currentTarget action)
        (unwrap state currentTarget . handler)
    put nextState
    case result of
        Right a -> return a
        Left e -> left e


-- | Like outOfDate, but does not give a memoized monitor input value.
triggerTarget :: String -> MonitorM monitorInput a
triggerTarget msg = triggerTargetInternal msg Nothing

-- | Says that the target is out of date. Triggers that the target
-- will be run. A monitorInput (e.g. a timestamp that says when the
-- input was created) has to be provided that can/should be used in
-- the closing monitor (see also memoizeMonitorInput).
outOfDate :: String -> monitorInput -> MonitorM monitorInput a
outOfDate msg monitorInput = triggerTargetInternal msg (Just monitorInput)

triggerTargetInternal :: String -> Maybe monitorInput -> MonitorM monitorInput a
triggerTargetInternal msg monitorInput = ActionM $ do
    currentTarget <- ask
    let error = Error currentTarget ("message from monitor: " ++ msg)
    logMessage $ showError error
    left $ OutDated error monitorInput

discardMonitorInput :: MonitorM monitorInput a -> TargetM a
discardMonitorInput (ActionM action) = ActionM $
    bimapEitherT (fmap (const ())) id action


bracketWithMonitor :: (Maybe monitorInput -> MonitorM monitorInput ())
    -> TargetM () -> TargetM ()
bracketWithMonitor monitor (ActionM action) = ActionM $ do
    currentState <- get
    currentTarget <- ask
    -- running the opening monitor
    (result, nextState) <- liftIO $ unwrap currentState currentTarget (monitor Nothing)
    put nextState
    case result of
        (Left (OutDated _ monitorInput)) -> do
            action
            getActionM $ discardMonitorInput (monitor monitorInput)
        (Left (ErrorShortCut error)) ->
            left $ ErrorShortCut error
        (Right ()) -> return ()

memoizeMonitorInput :: Maybe monitorInput -> MonitorM x monitorInput
    -> MonitorM x monitorInput
memoizeMonitorInput Nothing action = action
memoizeMonitorInput (Just input) _ = return input
