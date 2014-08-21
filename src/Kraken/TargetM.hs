{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, ScopedTypeVariables, DeriveFunctor #-}

module Kraken.TargetM (
    TargetName(..),
    showError,

    Error(..),
    TargetM,
    cancel,
    runTargetM,

    withTargetName,

    isolate,
    catch,
    mapExceptions,

    outOfDate,
    discardMonitorInput,
    bracketWithMonitor,
    memoizeMonitorInput,
  ) where


import           Control.Applicative
import qualified Control.Exception          as E
import           Control.Exception.Enclosed
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State        (StateT, get, put, runStateT)
import           Control.Monad.Trans.Either
import           Data.Monoid
import           Data.String

import           Kraken.Util


newtype TargetName = TargetName String
  deriving (Eq, Ord, IsString, Monoid)

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
  | OutDated Error monitorInput
    deriving (Functor)

type State = [Error]


-- | Our monad to run and monitor targets.
data TargetM monitorInput a = TargetM {
    getTargetM :: EitherT (ShortCut monitorInput) (StateT State (ReaderT (Maybe TargetName) IO)) a
  }
    deriving (Functor)

instance Applicative (TargetM monitorInput) where
    pure = TargetM . pure
    TargetM f <*> TargetM x = TargetM (f <*> x)

instance Monad (TargetM monitorInput) where
    TargetM a >>= f = TargetM $ do
        x <- a
        let (TargetM b) = f x
        b
    return = TargetM . return
    fail msg = do
        logMessageLn "TargetM.fail is discouraged: please, use cancel"
        cancel msg

cancel :: String -> TargetM monitorInput a
cancel msg = TargetM $ do
    target <- ask
    let error = Error target msg
    logMessage $ showError error
    left $ ErrorShortCut error

instance MonadIO (TargetM monitorInput) where
    liftIO = TargetM . liftIO

unwrap :: State -> Maybe TargetName -> TargetM monitorInput a -> IO (Either (ShortCut monitorInput) a, State)
unwrap state currentTarget (TargetM action) =
    runReaderT (runStateT (runEitherT action) state) currentTarget


runTargetM :: TargetM monitorInput a -> IO (Either [Error] a)
runTargetM action = do
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
withTargetName :: TargetName -> TargetM monitorInput a -> TargetM monitorInput a
withTargetName name (TargetM action) =
    TargetM $ local (const $ Just name) action


-- | Runs the given action and always succeeds. Both Lefts and Exceptions are
-- being written to the error state.
isolate :: TargetM monitorInput () -> TargetM monitorInput ()
isolate action = do
    currentTarget <- TargetM ask
    result <- liftIO $ catchAny
        (runTargetM ((maybe id withTargetName currentTarget) action))
        (\ e -> do
            let error = Error currentTarget (show e)
            logMessage $ showError error
            return $ Left [error])
    case result of
        Right () -> return ()
        Left errs -> TargetM $ do
            state <- get
            put (state ++ errs)
            return ()

mapExceptions :: (E.Exception a, E.Exception b) => (a -> b) -> TargetM monitorInput o -> TargetM monitorInput o
mapExceptions f action = catch action (liftIO . E.throwIO . f)

catch :: E.Exception e => TargetM monitorInput a -> (e -> TargetM monitorInput a) -> TargetM monitorInput a
catch action handler = TargetM $ do
    state <- get
    currentTarget <- ask
    (result, nextState) <- liftIO $ E.catch
        (unwrap state currentTarget action)
        (unwrap state currentTarget . handler)
    put nextState
    case result of
        Right a -> return a
        Left e -> left e


outOfDate :: String -> monitorInput -> TargetM monitorInput a
outOfDate msg monitorInput = TargetM $ do
    currentTarget <- ask
    let error = Error currentTarget ("message from monitor: " ++ msg)
    logMessage $ showError error
    left $ OutDated error monitorInput

discardMonitorInput :: TargetM monitorInput a -> TargetM () a
discardMonitorInput (TargetM action) = TargetM $
    bimapEitherT (fmap (const ())) id action


bracketWithMonitor :: (Maybe monitorInput -> TargetM monitorInput ())
    -> TargetM () () -> TargetM () ()
bracketWithMonitor monitor (TargetM action) = TargetM $ do
    currentState <- get
    currentTarget <- ask
    -- running the opening monitor
    (result, nextState) <- liftIO $ unwrap currentState currentTarget (monitor Nothing)
    put nextState
    case result of
        (Left (OutDated _ monitorInput)) -> do
            action
            getTargetM $ discardMonitorInput (monitor (Just monitorInput))
        (Left (ErrorShortCut error)) ->
            left $ ErrorShortCut error
        (Right ()) -> return ()

memoizeMonitorInput :: Maybe monitorInput -> TargetM x monitorInput
    -> TargetM x monitorInput
memoizeMonitorInput Nothing action = action
memoizeMonitorInput (Just input) _ = return input
