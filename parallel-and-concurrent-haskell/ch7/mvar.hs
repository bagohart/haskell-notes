import Control.Concurrent
import Control.Monad
import System.IO
import Text.Printf

-- MVar is a box that is full or empty.
-- it can be created full or empty.
-- you can take out (remove) a value from the MVar, this blocks iff it is empty
-- you can put a value into the MVar, this blocks iff full
-- use cases:
-- channel with one cell
-- shared mutable state with implicit lock
-- only a lock, while the shared mutable state is in C code or the filesystem.
-- lego for more complicated concurrency voodoo

main1 :: IO ()
main1 = do
    m <- newEmptyMVar
    forkIO $ putMVar m 'x' -- a new thread puts an 'x' into the box
    r <- takeMVar m
    print r

main2 :: IO ()
main2 = do
    m <- newEmptyMVar
    forkIO $ do
        putMVar m 'x'
        threadDelay (10^6 * 1)
        putMVar m 'y'
    r <- takeMVar m
    print r
    r' <- takeMVar m
    print r'

main3 :: IO ()
main3 = do
    m <- newEmptyMVar
    takeMVar m
-- in this case, there is automatic deadlock detection and an exception is thrown. cool.
