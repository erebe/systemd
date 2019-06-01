{-# LANGUAGE ForeignFunctionInterface #-}
module System.Systemd.Internal where

import           Control.Exception         (bracket)
import           Control.Monad
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8     as BC
import           Data.ByteString.Unsafe    (unsafeUseAsCStringLen)
import           Data.List
import           Foreign.C.Types           (CInt (..))
import           Foreign.Marshal           (free, mallocBytes)
import           Foreign.Ptr
import           Network.Socket
import           Network.Socket.Address    hiding (recvFrom, sendTo)
import           Network.Socket.ByteString
import           System.Posix.Env
import           System.Posix.Types        (Fd (..))

envVariableName :: String
envVariableName = "NOTIFY_SOCKET"

foreign import ccall unsafe "sd_notify_with_fd"
  c_sd_notify_with_fd :: CInt -> Ptr a -> CInt -> Ptr b -> CInt -> CInt -> IO CInt

-- | Unset all environnement variable related to Systemd.
--
-- Calls to functions like 'System.Systemd.Daemon.notify' and
-- 'System.Systemd.Daemon.getActivatedSockets' will return
-- 'Nothing' after that.
unsetEnvironnement :: IO ()
unsetEnvironnement = mapM_ unsetEnv [envVariableName, "LISTEN_PID", "LISTEN_FDS", "LISTEN_FDNAMES"]

sendBufWithFdTo :: Socket -> BC.ByteString -> SockAddr -> Fd -> IO Int
sendBufWithFdTo sock state addr fdToSend =
  unsafeUseAsCStringLen state $ \(ptr, nbytes) ->
    bracket addrPointer free $ \p_addr -> do
      fd <- socketToFd sock
      fromIntegral <$> c_sd_notify_with_fd (fromIntegral fd) ptr (fromIntegral nbytes)
                                           p_addr (fromIntegral addrSize) (fromIntegral fdToSend)
  where addrSize = sizeOfSocketAddress addr
        addrPointer = mallocBytes addrSize >>= (\ptr -> pokeSocketAddress ptr addr >> pure ptr)

notifyWithFD_ :: Bool -> String -> Maybe Fd -> IO (Maybe ())
notifyWithFD_ unset_env state fd = do
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
            nbBytes  <- liftIO $ case fd of
                  Nothing     -> sendTo socketFd (BC.pack state) (SockAddrUnix socketPath')
                  Just sock'  -> sendBufWithFdTo socketFd (BC.pack state)
                                                (SockAddrUnix socketPath') sock'

            liftIO $ close socketFd
            guard $ nbBytes >= length state


            return ()

socketToFd :: Socket -> IO Fd
socketToFd = fmap Fd . fdSocket

fdToSocket :: Fd -> IO Socket
fdToSocket = mkSocket . fromIntegral
