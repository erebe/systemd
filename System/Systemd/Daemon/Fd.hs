{-|
Module      : System.Systemd.Daemon.Fd
Description : File descriptor based socket activation/management
              using systemd
Copyright   : (c) Romain Gérard, 2014
                  David Fisher, 2013
                  Lukas Epple, 2019
License     : BSD3
Maintainer  : romain.gerard@erebe.eu
Stability   : stable
Portability : Requires Systemd or will fail otherwise

This module implements all functions from "System.Systemd.Daemon"
that require or return 'Network.Socket.Socket's purely using 'Fd's.
This is especially useful if you have to do low level IO using
file descriptors or use a different socket library than @network@.

The API is exactly the same as "System.Systemd.Daemon" except that
'Network.Socket.Socket's have been replaced by 'Fd's (actually
"System.Systemd.Daemon" uses this module internally). This also means
that "System.Systemd.Daemon.Fd" and "System.Systemd.Daemon" expose
conflicting functions. You either have to import "System.Systemd.Daemon.Fd"
@qualified@ or like so:

@
import System.Systemd.Daemon hiding ( notifyWithFD, storeFd
                                    , storeFdWithName
                                    , getActivatedSockets
                                    , getActivatedSocketsWithNames )
import System.Systemd.Daemon.Fd
@

The functions in "System.Systemd.Daemon" that are not implemented
in this module are 100% compatible with "System.Systemd.Daemon.Fd".
-}
module System.Systemd.Daemon.Fd
  ( -- * Notify functions
    notifyWithFD
  , storeFd
  , storeFdWithName
    -- * Socket activation functions
  , getActivatedSockets
  , getActivatedSocketsWithNames
  ) where

import           Control.Monad
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8     as BC
import           Foreign.C.Types           (CInt (..))
import           Network.Socket            (setNonBlockIfNeeded)
import           System.Posix.Env          (getEnv)
import           System.Posix.Process
import           System.Posix.Types        (Fd (..))
import           System.Systemd.Internal

fdStart :: CInt
fdStart = 3

-- | Notify Systemd to store a file descriptor for us. This together
--   with 'getActivatedSockets' allows for zero downtime
--   restarts and socket activation.
--
--   Equivalent to standard 'System.Systemd.Daemon.storeFd'
storeFd :: Fd -> IO (Maybe ())
storeFd = notifyWithFD False "FDSTORE=1"

-- | Like 'storeFd', but associate the file descriptor with a name.
--   Best used along with 'getActivatedSocketsWithNames'.
--
--   Equivalent to standard 'System.Systemd.Daemon.storeFdWithName'
storeFdWithName :: Fd -> String -> IO (Maybe ())
storeFdWithName fd name = notifyWithFD False ("FDSTORE=1\nFDNAME=" ++ name) fd

-- | Same as 'System.Systemd.Daemon.notify', but send along a 'Fd'.
--   Note that the caller must set the message, i. e. send @FDSTORE=1@
--   to actually store the file descriptor. In most cases it is probably best
--   to use 'storeFd' or the notify-functions from "System.Systemd.Daemon".
--
--   Equivalent to standard 'System.Systemd.Daemon.notifyWithFD'.
notifyWithFD :: Bool -> String -> Fd -> IO (Maybe ())
notifyWithFD unset_env state sock = notifyWithFD_ unset_env state (Just sock)

-- | Return 'Just' a list of file descriptors if the current process
--   has been activated with one or more socket by systemd, 'Nothing'
--   otherwise.
--
--   The file descriptors are in the same order as the sockets in the
--   associated @.socket@ file. The sockets will have their family, type,
--   and status set according to the @.socket@ file.
--
--   Equivalent to standard 'System.Systemd.Daemon.getActivatedSockets'
getActivatedSockets :: IO (Maybe [Fd])
getActivatedSockets = runMaybeT $ do
    listenPid     <- read <$> MaybeT (getEnv "LISTEN_PID")
    listenFDs     <- read <$> MaybeT (getEnv "LISTEN_FDS")

    myPid <- liftIO getProcessID
    guard $ listenPid == myPid

    mapM (\fd -> liftIO (setNonBlockIfNeeded fd) >> pure (Fd fd))
         [fdStart .. fdStart + listenFDs - 1]

-- | Like 'getActivatedSockets', but also return the associated names.
--   If a file descriptor has no associated name, it will be a generic
--   one set by systemd.
--
--   Equivalent to standard 'System.Systemd.Daemon.getActivatedSocketsWithNames'
getActivatedSocketsWithNames :: IO (Maybe [(Fd, String)])
getActivatedSocketsWithNames = runMaybeT $ do
    listenFDNames <- MaybeT (getEnv "LISTEN_FDNAMES")
    let listenFDNames' = fmap BC.unpack $ BC.split ':' $ BC.pack listenFDNames

    nonBlockFds <- MaybeT getActivatedSockets
    guard $ length nonBlockFds == length listenFDNames'

    return $ zip nonBlockFds listenFDNames'
