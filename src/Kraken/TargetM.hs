{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, ScopedTypeVariables #-}

module Kraken.TargetM (
    TargetName(..),
    showError,

    Error(..),
    TargetM,
    runTargetM,

    withTargetName,

    isolate,
    catch,
    mapExceptions,

    outOfDate,
    bracketWithMonitor,
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


data ShortCut
  = ErrorShortCut Error
  | OutDated Error

type State = [Error]


-- | Our monad to run and monitor targets.
newtype TargetM a = TargetM (EitherT ShortCut (StateT State (ReaderT (Maybe TargetName) IO)) a)
  deriving (Functor, Applicative, MonadIO)

instance Monad TargetM where
    TargetM a >>= f = TargetM $ do
        x <- a
        let (TargetM b) = f x
        b
    return = TargetM . return
    fail msg = TargetM $ do
        target <- ask
        let error = Error target msg
        logMessage $ showError error
        left $ ErrorShortCut error

unwrap :: State -> Maybe TargetName -> TargetM a -> IO (Either ShortCut a, State)
unwrap state currentTarget (TargetM action) =
    runReaderT (runStateT (runEitherT action) state) currentTarget


runTargetM :: TargetM a -> IO (Either [Error] a)
runTargetM action = do
    (result, state) <- unwrap [] Nothing action
    return $ case (result, state) of
        (Right x, []) -> Right x
        (Right _, errors) -> Left errors
        (Left (ErrorShortCut error), errors) -> Left (errors ++ [error])
        (Left (OutDated error), errors) ->
            -- OutDated treated like an error, because it's not being executed
            -- inside bracketWithMonitor.
            Left (errors ++ [error])

-- | Sets the TargetName that will be included in the error messages that
-- can are raised by 'fail' and 'outOfDate'.
withTargetName :: TargetName -> TargetM a -> TargetM a
withTargetName name (TargetM action) =
    TargetM $ local (const $ Just name) action


-- | Runs the given action and always succeeds. Both Lefts and Exceptions are
-- being written to the error state.
isolate :: TargetM () -> TargetM ()
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

mapExceptions :: (E.Exception a, E.Exception b) => (a -> b) -> TargetM o -> TargetM o
mapExceptions f action = catch action (liftIO . E.throwIO . f)

catch :: E.Exception e => TargetM a -> (e -> TargetM a) -> TargetM a
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


outOfDate :: String -> TargetM a
outOfDate msg = TargetM $ do
    currentTarget <- ask
    let error = Error currentTarget ("message from monitor: " ++ msg)
    logMessage $ showError error
    left $ OutDated error

bracketWithMonitor :: TargetM () -> TargetM () -> TargetM ()
bracketWithMonitor (TargetM monitor) (TargetM action) = TargetM $ do
    currentState <- get
    currentTarget <- ask
    -- running the monitor
    (result, nextState) <- liftIO $ unwrap currentState currentTarget (TargetM monitor)
    put nextState
    case result of
        (Left (OutDated _)) -> do
            action
            monitor
        (Left (ErrorShortCut error)) ->
            left $ ErrorShortCut error
        (Right ()) -> return ()
