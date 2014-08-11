{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses #-}

module Kraken.TargetM (
    TargetName(..),
    TargetM,
    runTargetM,
    runTargetMSilently,
    abort,
    logMessage,
    logMessageLn,
    withTargetName,
    showFailure,

    catch,
    bracket,
    (>>!),
  ) where


import           Control.Applicative
import           Control.Arrow              (first)
import qualified Control.Exception          as E
import           Control.Exception.Enclosed
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Data.Monoid
import           Data.String
import           System.IO


newtype TargetName = TargetName String
  deriving (Eq, Ord, IsString, Monoid)

instance Show TargetName where
    show (TargetName n) = n

-- | Our monad to run and monitor targets.
newtype TargetM a = TargetM (EitherT [(Maybe TargetName, String)] (ReaderT (Maybe TargetName, Bool) IO) a)
    deriving (Functor, Applicative, Monad, MonadIO)

runTargetM :: TargetM a -> IO (Either [(Maybe TargetName, String)] a)
runTargetM action = runTargetMInternal True action

-- runs the given action silently, i.e.
-- - without outputting stuff to stderr and
-- - without using the monads failure mechanism, errors are returned as Lefts.
runTargetMSilently :: TargetM a -> TargetM (Either [(Maybe TargetName, String)] a)
runTargetMSilently action = liftIO $
    runTargetMInternal False action

runTargetMInternal :: Bool -> TargetM a -> IO (Either [(Maybe TargetName, String)] a)
runTargetMInternal printToStderr (TargetM action) = catchAny
    ((runReaderT (runEitherT action)) (Nothing, printToStderr))
    (\ e -> do
        when printToStderr $
            logMessage (showFailure (Nothing, show e))
        return $ Left [(Nothing, show e)])

targetM :: IO (Either [(Maybe TargetName, String)] a) -> TargetM a
targetM action = do
    result <- liftIO $ action
    case result of
        Left messages -> TargetM $ left messages
        Right x -> return x

abort :: String -> TargetM a
abort message = TargetM $ do
    (mTarget, printToStderr) <- ask
    let failure = (mTarget, message)
    when printToStderr $
        logMessage (showFailure failure)
    left [failure]

logMessage :: MonadIO m => String -> m ()
logMessage = liftIO . hPutStr stderr

logMessageLn :: MonadIO m => String -> m ()
logMessageLn = liftIO . hPutStrLn stderr

-- | Sets the TargetName that will be included in the error messages that
-- can are raised by 'abort'.
withTargetName :: TargetName -> TargetM a -> TargetM a
withTargetName name (TargetM action) =
    TargetM $ local (first $ const $ Just name) action

showFailure :: (Maybe TargetName, String) -> String
showFailure (mTarget, message) = unlines $
    ((maybe "<no target>" show mTarget) ++ ":") :
    (fmap ("    " ++) (lines message))


catch :: E.Exception e => TargetM a -> (e -> TargetM a) -> TargetM a
catch action handler =
    targetM $ liftIO $ E.catch (runTargetM action) $ \ e ->
        liftIO $ runTargetM $ handler e

bracket :: IO resource -> (resource -> IO ())
    -> (resource -> TargetM result)
    -> TargetM result
bracket acquire free action =
    TargetM $ EitherT $ ReaderT $ \ answer ->
        E.bracket acquire free (\ resource ->
            let TargetM (EitherT (ReaderT ioAction)) = action resource
            in ioAction answer)

(>>!) :: TargetM () -> TargetM () -> TargetM ()
a >>! b = TargetM $ do
    resultA <- liftIO $ runTargetM a
    resultB <- liftIO $ runTargetM b
    combineLefts resultA resultB
  where
    combineLefts :: (Monad m, Monoid a) => Either a () -> Either a () -> EitherT a m ()
    combineLefts a b = case (a, b) of
        (Right (), Right ()) -> right ()
        (Left messages, Right ()) -> left messages
        (Right (), Left messages) -> left messages
        (Left messagesA, Left messagesB) -> left (messagesA <> messagesB)
