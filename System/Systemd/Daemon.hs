{-|
Module      : System.Systemd.Daemon
Description : Systemd facilities to manage daemons
Copyright   : (c) Romain Gérard, 2014
                  David Fisher, 2013
License     : BSD3
Maintainer  : romain.gerard@erebe.eu
Stability   : stable
Portability : Require Systemd or will fail otherwise

Implementation of Systemd facilities to create and manage
daemons.

All socket-related actions in this module, work with the
"Network.Socket" module from @network@. If you want to use
a different socket library or work directly with file
descriptors, see "System.Systemd.Daemon.Fd".

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
                             , notifyWithFD
                             , storeFd
                             , storeFdWithName
                             , notifyWatchdog
                             , notifyReady
                             , notifyPID
                             , notifyErrno
                             , notifyStatus
                             , notifyBusError
                             , notifyReloading
                             , notifyStopping
                             -- * Socket activation functions
                             , getActivatedSockets
                             , getActivatedSocketsWithNames
                             -- * Utils
                             , unsetEnvironnement
                             ) where

import qualified System.Systemd.Daemon.Fd as Fd
import           System.Systemd.Internal

import           Foreign.C.Error          (Errno (..))
import           System.Posix.Types       (CPid (..))

import           Network.Socket


-- | Notify the watchdog that the program is still alive
notifyWatchdog :: IO (Maybe())
notifyWatchdog = notify False "WATCHDOG=1"

-- | Notify the systemd that the program is ready
notifyReady :: IO (Maybe())
notifyReady = notify False "READY=1"

-- | Notify systemd of the PID of the program (for after a fork)
notifyPID :: CPid -> IO (Maybe())
notifyPID pid = notify False $ "MAINPID=" ++ show pid

-- | Notify systemd that the service is reloading its configuration
notifyReloading :: IO (Maybe())
notifyReloading = notify False "RELOADING=1"

-- | Notify systemd that the service is beginning its shutdown
notifyStopping :: IO (Maybe())
notifyStopping = notify False "STOPPING=1"

-- | Notify systemd of an 'Errno' error
notifyErrno :: Errno -> IO (Maybe())
notifyErrno (Errno errorNb) = notify False $ "ERRNO=" ++ show errorNb

-- | Notify systemd of the status of the program.
--
-- An arbitrary 'String' can be passed
notifyStatus :: String -> IO (Maybe())
notifyStatus msg = notify False $ "STATUS=" ++ msg

-- | Notify systemd of a DBUS error like.
--
-- Correct formatting of the 'String' is left to the caller
notifyBusError :: String -> IO (Maybe())
notifyBusError msg = notify False $ "BUSERROR=" ++ msg

-- | Notify systemd to store a socket for us.
--
-- To be used along 'getActivatedSockets' during a restart
--
-- Usefull for zero downtime restart
storeFd :: Socket -> IO (Maybe ())
storeFd sock = socketToFd_ sock >>= Fd.storeFd

-- | Notify systemd to store a socket for us and specify a name.
--
-- To be used along 'getActivatedSocketsWithNames' during a restart
--
-- Usefull for zero downtime restart
storeFdWithName :: Socket -> String -> IO (Maybe ())
storeFdWithName sock name = socketToFd_ sock >>= flip Fd.storeFdWithName name

-- | Notify systemd about an event
--
-- After notifying systemd the 'Bool' parameter specify if the environnement
-- shall be unset (Further call to notify will fail)
--
-- The 'String' is the event to pass
--
-- Returns 'Nothing' if the program was not started with systemd
-- or that the environnement was previously unset
notify :: Bool -> String -> IO (Maybe ())
notify unset_env state = notifyWithFD_ unset_env state Nothing

-- | Same as 'notify' but send along a socket to be stored
--
-- It is up to the caller to properly set the message
-- (i.e: do not forget to set FDSTORE=1)
notifyWithFD :: Bool -> String -> Socket -> IO (Maybe ())
notifyWithFD unset_env state sock = socketToFd_ sock >>= Fd.notifyWithFD unset_env state

------------------------------------------------------------------------------------------------
--  SOCKET
------------------------------------------------------------------------------------------------

-- | Return a list of activated sockets, if the program was started with
-- socket activation.
--
-- The sockets are in the same order as in the associated @.socket@ file.
-- The sockets will have their family, type, and status set appropriately.
--
-- Returns 'Nothing' in systems without socket activation (or
-- when the program was not socket activated).
getActivatedSockets :: IO (Maybe [Socket])
getActivatedSockets = Fd.getActivatedSockets >>= traverse (mapM fdToSocket)

-- | Same as 'getActivatedSockets' but return also the names associated
-- with those sockets if 'storeFdWithName' was used or specified in the @.socket@ file.
--
-- IF 'storeFd' was used to transmit the socket to systemd, the name will be a generic one
-- (i.e: usally "stored")
getActivatedSocketsWithNames :: IO (Maybe [(Socket, String)])
getActivatedSocketsWithNames = Fd.getActivatedSocketsWithNames >>= traverse (mapM socketWithName)
  where socketWithName (fd, name) = fmap (flip (,) name) $ fdToSocket fd
