
import System.Systemd.Daemon
import Control.Monad
import Control.Concurrent

import qualified Data.ByteString.Char8      as BC


main :: IO ()
main = forever runner
    where
        runner = do
            res <- notify False $ BC.pack "WATCHDOG=1"
            print res
            threadDelay $ 1000000 * 2

