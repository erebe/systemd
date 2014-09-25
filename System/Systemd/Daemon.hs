{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : System.Systemd.Daemon
Description : Systemd facilities to manage daemons
Copyright   : (c) Romain Gérard, 2014
                  David Fisher, 2013
License     : BSD3
Maintainer  : romain.gerard@erebe.eu
Stability   : experimental
Portability : Require Systemd or will fail otherwise

Implementation of Systemd facilities to create and manage
daemons.

This module contains socket activation and notify tools. See 

* <http://0pointer.de/blog/projects/socket-activation.html> 
* <http://www.freedesktop.org/software/systemd/man/systemd.socket.html>
* <http://www.freedesktop.org/software/systemd/man/systemd.service.html>

Example:

@
import Control.Monad(forever)
import System.Systemd.Daemon(notifyWatchdog)

main :: IO ()
main = forever $ do
        functionThatMayHang
        notifyWatchdog
@

If you use the service described as below,
Systemd will restart your program each time the watchdog 
fail to notify itself under 60 sec.

@
[Unit]
Description=MyDaemon

[Service]
Type=simple
TimeoutStartSec=0
ExecStart=AbsolutePathToMyExecutable
WatchdogSec=60
Restart=on-failure

[Install]
WantedBy=multi-user.target
@
-}

module System.Systemd.Daemon (
                               -- * Notify functions
                               notify
                             , notifyWatchdog
                             , notifyReady
                             , notifyPID
                             , notifyErrno
                             , notifyStatus
                             , notifyBusError
                             -- * Socket activation functions
                             , getActivatedSockets
                             -- * Utils
                             , unsetEnvironnement
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



envVariableName :: String
envVariableName = "NOTIFY_SOCKET"

-- | Notify the watchdog that the program is still alive
notifyWatchdog :: IO (Maybe())
notifyWatchdog = notify False "WATCHDOG=1"

-- | Notify the systemd that the program is ready
notifyReady :: IO (Maybe())
notifyReady = notify False "READY=1"

-- | Notify systemd of the PID of the program (for after a fork)
notifyPID :: CPid -> IO (Maybe())
notifyPID pid = notify False $ "MAINPID=" ++ show pid

-- | Notify systemd of an 'Errno' error
notifyErrno :: Errno -> IO (Maybe())
notifyErrno (Errno errorNb) = notify False $ "ERRNO=" ++ show errorNb

-- | Notify systemd of the status of the program. An arbitrary 'String'
-- can be passed
notifyStatus :: String -> IO (Maybe())
notifyStatus msg = notify False $ "STATUS=" ++ msg

-- | Notify systemd of a DBUS error like.
-- Correct formatting of the 'String' is left to the caller
notifyBusError :: String -> IO (Maybe())
notifyBusError msg = notify False $ "BUSERROR=" ++ msg

-- | Unset all environnement variable related to Systemd.
-- Calls to 'notify' like and 'getActivatedSockets' functions will return 'Nothing' after that
unsetEnvironnement :: IO ()
unsetEnvironnement = mapM_ unsetEnv [envVariableName, "LISTEN_PID", "LISTEN_FDS"]

-- | Notify systemd about an event
-- After notifying systemd the 'Bool' parameter specify if the environnement
-- shall be unset (Further call to notify will fail)
-- The @String@ is the event to pass
-- Returns @Nothing@ if the program was not started with systemd
-- or that the environnement was previously unset
notify :: Bool -> String -> IO (Maybe ())
notify unset_env state = do
        res <- runMaybeT notifyImpl
        when unset_env unsetEnvironnement
        return res

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
