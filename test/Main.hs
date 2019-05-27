
import System.Systemd.Daemon
import Control.Monad
import Control.Concurrent

import Network.Socket
import System.IO
import Data.Char
import System.Posix.Env as Ev


apF :: Show w => w -> IO ()
apF = appendFile "/tmp/log" . (++ "\n") . show

test :: IO ()
test = do
  hSetBuffering stdout LineBuffering
  ev <-  Ev.getEnvironment
  apF ev

  apF "totot"
  ev' <- getActivatedSocketsWithNames
  apF ev'
  apF "totot"

  threadDelay $ 1000000 * 20
  s <- socket AF_INET Stream defaultProtocol
  s' <- socket AF_INET Stream defaultProtocol
  listen s 1213
  listen s' 1214

  x <- storeFd s
  apF x
  x <- storeFdWithName s' "tutu"
  apF x
  forever (runner s)
    where
        runner s = do
            res <- notifyWatchdog
            x <- notifyWithFD False "FDSTORE=1" s
            apF x
            threadDelay $ 1000000 * 2

main :: IO ()
main = do
   -- _ <- test
   return ()
