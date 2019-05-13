
import System.Systemd.Daemon
import Control.Monad
import Control.Concurrent
import Network

import System.IO
import Data.Char
import System.Posix.Env as Ev


apF :: Show w => w -> IO ()
apF = appendFile "/home/erebe/log" . (++ "\n") . show

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
  s <- listenOn (PortNumber 1213)
  s' <- listenOn (PortNumber 1214)

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
