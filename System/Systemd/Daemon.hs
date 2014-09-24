
module System.Systemd.Daemon ( notify
                             , notifyWatchdog
                             , notifyReady
                             , notifyPID
                             , notifyErrno
                             , notifyStatus
                             , notifyBusError
                             , unsetEnvironnement
                             ) where

import           Control.Monad
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Maybe
import           Data.List

import qualified Data.ByteString.Char8     as BC

import           Foreign.C.Error           (Errno (..))
import           System.Posix.Env
import           System.Posix.Types        (CPid (..))

import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString


envVariableName :: String
envVariableName = "NOTIFY_SOCKET"

notifyWatchdog :: IO Bool
notifyWatchdog = notify False "WATCHDOG=1"

notifyReady :: IO Bool
notifyReady = notify False "READY=1"

notifyPID :: CPid -> IO Bool
notifyPID pid = notify False $ "MAINPID=" ++ show pid

notifyErrno :: Errno -> IO Bool
notifyErrno (Errno errorNb) = notify False $ "ERRNO=" ++ show errorNb

notifyStatus :: String -> IO Bool
notifyStatus msg = notify False $ "STATUS=" ++ msg

notifyBusError :: String -> IO Bool
notifyBusError msg = notify False $ "BUSERROR=" ++ msg

unsetEnvironnement :: IO ()
unsetEnvironnement = unsetEnv envVariableName

notify :: Bool -> String -> IO Bool
notify unset_env state = do
        res <- runMaybeT notifyImpl
        when unset_env unsetEnvironnement
        case res of
            Just _ -> return True
            _      -> return False

    where
        isValidPath path =   (length path >= 2)
                          && ( "@" `isPrefixOf` path
                             || "/" `isPrefixOf` path)
        notifyImpl = do
            guard $ state /= ""

            socketPath <- MaybeT (getEnv envVariableName)
            guard $ isValidPath socketPath
            let socketPath' = if head socketPath == '@' -- For abstract socket
                              then '\0' : tail socketPath
                              else socketPath

            socketFd <- liftIO $ socket AF_UNIX Datagram 0
            nbBytes  <- liftIO $ sendTo socketFd (BC.pack state) (SockAddrUnix socketPath')
            liftIO $ close socketFd
            guard $ nbBytes >= length state


            return ()
