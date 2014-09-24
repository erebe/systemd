
module System.Systemd.Daemon (notify) where

import           Control.Monad.Trans.Maybe
import           Control.Monad
import           Control.Monad.IO.Class(liftIO)
import           Data.List

-- import qualified Data.Text                 as T
import qualified Data.ByteString            as B
-- import qualified Data.ByteString.Char8      as BC

import           System.Posix.Env
import           Control.Applicative
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

notify :: Bool -> B.ByteString -> IO Bool
notify unset_env state = do
        res <- runMaybeT notifyImpl
        when unset_env unsetEnvironnement
        case res of
            Just _ -> return True
            _      -> return False

    where
        envName = "NOTIFY_SOCKET"
        isValidPath path =   (length path >= 2)
                          && ( "@" `isPrefixOf` path
                             || "/" `isPrefixOf` path)
        unsetEnvironnement = unsetEnv envName
        notifyImpl = do
            guard $ state /= B.empty

            socketPath <- MaybeT (getEnv envName) 
            guard $ isValidPath socketPath

            socketFd <- liftIO $ socket AF_UNIX Datagram 0
            nbBytes <- liftIO $ sendTo socketFd state (SockAddrUnix socketPath)
            liftIO $ close socketFd
            guard $ nbBytes >= B.length state


            return ()
