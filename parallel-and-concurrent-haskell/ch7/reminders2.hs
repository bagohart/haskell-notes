import Control.Concurrent
import Control.Monad
import System.IO
import Text.Printf

main = loop
    where 
        loop = do
            s <- getLine
            if s == "exit"
               then return ()
               else do forkIO $ setReminder s
                       loop
                       -- according to the book, the program exits as soon as main exits,
                       -- even if there are still threads running.
                       -- I observe that in ghci it just continues :)

setReminder :: String -> IO ()
setReminder s = do
    let t = read s :: Int
    printf "Ok, I'll remind you in %d seconds\n" t
    threadDelay (10^6 * t)
    printf "%d seconds is up! BING! \BEL\n" t

-- how do I wait for a thread to return? This is seen as a higher-level feature. hm.
