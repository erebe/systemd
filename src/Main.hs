
import System.Systemd.Daemon
import Control.Monad
import Control.Concurrent



main :: IO ()
main = forever runner
    where
        runner = do
            res <- notifyWatchdog
            print res
            threadDelay $ 1000000 * 2

