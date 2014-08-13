{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses #-}

module Kraken.TargetM (
    TargetName(..),
    showFailure,

    TargetM,
    runTargetM,
    runTargetMSilently,
    withTargetName,
    isolate,
    abort,

    catch,
  ) where


import           Control.Applicative
import           Control.Arrow              (first)
import qualified Control.Exception          as E
import           Control.Exception.Enclosed
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Control.Monad.Writer
import           Data.String

import           Kraken.Util


newtype TargetName = TargetName String
  deriving (Eq, Ord, IsString, Monoid)

instance Show TargetName where
    show (TargetName n) = n

showFailure :: (Maybe TargetName, String) -> String
showFailure (mTarget, message) = unlines $
    ((maybe "<no target>" show mTarget) ++ ":") :
    (fmap ("    " ++) (lines message))


type Errors = [(Maybe TargetName, String)]

-- | Our monad to run and monitor targets.
newtype TargetM a = TargetM (EitherT Errors (WriterT Errors (ReaderT (Maybe TargetName, Bool) IO)) a)
    deriving (Functor, Applicative, Monad, MonadIO)

runTargetM :: TargetM a -> IO (Either [(Maybe TargetName, String)] a)
runTargetM action = runTargetMInternal True action

-- runs the given action silently, i.e.
-- - without outputting stuff to stderr and
-- - without using the monads failure mechanism, errors are returned as Lefts.
runTargetMSilently :: TargetM a -> IO (Either Errors a)
runTargetMSilently action =
    runTargetMInternal False action

runTargetMInternal :: Bool -> TargetM a -> IO (Either Errors a)
runTargetMInternal printToStderr (TargetM action) = do
    result <- (runReaderT (runWriterT (runEitherT action))) (Nothing, printToStderr)
    return $ case result of
        (Right a, []) -> Right a
        (Right _, collectedErrors) -> Left $ collectedErrors
        (Left errors, collectedErrors) -> Left (collectedErrors ++ errors)

targetM :: IO (Either [(Maybe TargetName, String)] a) -> TargetM a
targetM action = do
    result <- liftIO $ action
    case result of
        Left messages -> TargetM $ left messages
        Right x -> return x

-- | Sets the TargetName that will be included in the error messages that
-- can are raised by 'abort'.
withTargetName :: TargetName -> TargetM a -> TargetM a
withTargetName name (TargetM action) =
    TargetM $ local (first $ const $ Just name) action

-- | Runs the given action and always succeeds. Both Lefts and Exceptions are
-- being written to the error state.
isolate :: TargetM () -> TargetM ()
isolate action = do
    (_, printToStderr) <- TargetM $ ask
    result <- liftIO $ catchAny
        (runTargetMInternal printToStderr action)
        (\ e -> do
            logMessage $ showFailure (Nothing, show e)
            return $ Left [(Nothing, show e)])
    case result of
        Left errors -> TargetM $ tell errors
        Right () -> return ()

abort :: String -> TargetM a
abort message = TargetM $ do
    (mTarget, printToStderr) <- ask
    let failure = (mTarget, message)
    when printToStderr $
        logMessage (showFailure failure)
    left [failure]

catch :: E.Exception e => TargetM a -> (e -> TargetM a) -> TargetM a
catch action handler =
    targetM $ liftIO $ E.catch (runTargetM action) $ \ e ->
        liftIO $ runTargetM $ handler e
