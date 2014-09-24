{-# LANGUAGE ForeignFunctionInterface #-}

module System.Systemd.Daemon ( notify
                             , notifyWatchdog
                             , notifyReady
                             , notifyPID
                             , notifyErrno
                             , notifyStatus
                             , notifyBusError
                             , unsetEnvironnement
                             , getActivatedSockets
                             ) where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Maybe
import           Data.List

import qualified Data.ByteString.Char8     as BC

import           Foreign.C.Error           (Errno (..))
import           Foreign.C.Types           (CInt (..))
import           System.Posix.Env
import           System.Posix.Process
import           System.Posix.Types        (CPid (..))

import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString


------------------------------------------------------------------------------------------------
--  NOTIFY
------------------------------------------------------------------------------------------------
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
                     >> unsetEnv "LISTEN_PID"
                     >> unsetEnv "LISTEN_FDS"

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



------------------------------------------------------------------------------------------------
--  SOCKET
------------------------------------------------------------------------------------------------

fdStart :: CInt
fdStart = 3

-- | Return a list of activated sockets, if the program was started with
-- socket activation.  The sockets are in the same order as in
-- the associated @.socket@ file.  The sockets will have their family, type,
-- and status set appropriately.  Returns @Nothing@ in systems without socket activation (or
-- when the program was not socket activated).
getActivatedSockets :: IO (Maybe [Socket])
getActivatedSockets = runMaybeT $ do
    listenPid <- read <$> MaybeT (getEnv "LISTEN_PID")
    listenFDs <- read <$> MaybeT (getEnv "LISTEN_FDS")
    myPid     <- liftIO getProcessID
    guard $ listenPid == myPid
    mapM makeSocket [fdStart .. fdStart + listenFDs - 1]
  where makeSocket :: CInt -> MaybeT IO Socket
        makeSocket fd = do
          fam  <- socketFamily fd
          typ  <- socketType fd
          stat <- socketStatus fd
          liftIO $ mkSocket fd fam typ defaultProtocol stat

socketFamily :: CInt -> MaybeT IO Family
socketFamily fd = do
    familyInt <- liftIO $ c_socket_family fd
    guard $ familyInt >= 0
    return $ unpackFamily familyInt

socketType :: CInt -> MaybeT IO SocketType
socketType fd = do
    typeInt <- liftIO $ c_socket_type fd
    case typeInt of
        0 -> return NoSocketType
        1 -> return Stream
        2 -> return Datagram
        3 -> return Raw
        4 -> return RDM
        5 -> return SeqPacket
        _ -> mzero

socketStatus :: CInt -> MaybeT IO SocketStatus
socketStatus fd = do
    listeningInt <- liftIO $ c_socket_listening fd
    case listeningInt of
      0 -> return Bound
      1 -> return Listening
      _ -> mzero

foreign import ccall unsafe "socket_family"
  c_socket_family :: CInt -> IO CInt

foreign import ccall unsafe "socket_type"
  c_socket_type :: CInt -> IO CInt

foreign import ccall unsafe "socket_listening"
  c_socket_listening :: CInt -> IO CInt
