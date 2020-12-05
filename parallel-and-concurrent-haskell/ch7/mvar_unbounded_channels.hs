import Control.Concurrent

newChan' :: IO (Chan' a)
newChan' = do
    hole <- newEmptyMVar
    readVar  <- newMVar hole -- I build two new MVars, putting the empty MVar in both...
    writeVar <- newMVar hole
    return (Chan' readVar writeVar)
-- this channel is empty.

readChan' :: Chan' a -> IO a
readChan' (Chan' readVar _) = do
    stream <- takeMVar readVar -- this always returns instantly
    -- Item val tail <- takeMVar stream -- this pattern match cannot fail. it can block though, because empty hole here.
    Item val tail <- readMVar' stream -- we need this for dupChan' to work correctly, otherwise eternal block.
    putMVar readVar tail -- update read pointer
    return val

writeChan' :: Chan' a -> a -> IO ()
writeChan' (Chan' _ writeVar) val = do
    newHole <- newEmptyMVar -- I write into existing MVar that is the hole. So afterwards I will need a new hole.
    oldHole <- takeMVar writeVar -- get the MVar from the write pointer
    putMVar oldHole (Item val newHole) -- old hole was empty, put value into it and link to new hole
    putMVar writeVar newHole -- update write pointer

dupChan' :: Chan' a -> IO (Chan' a)
dupChan' (Chan' _ writeVar) = do
    hole <- readMVar' writeVar -- the new channel begins empty
    newReadVar <- newMVar hole -- the channels have separate readVars
    return $ Chan' newReadVar writeVar

-- push a value back on the read end of the channel
unGetChan' :: Chan' a -> a -> IO ()
unGetChan' (Chan' readVar _) val = do
    newReadEnd <- newEmptyMVar
    readEnd <- takeMVar readVar
    putMVar newReadEnd (Item val readEnd)
    putMVar readVar newReadEnd
-- this works, but deadlocks if another thread is already blocking the channel by trying to read from it
-- :'(
-- MVar is tricky. we cannot fix this with this structure u_U

readMVar' :: MVar a -> IO a
readMVar' m = do
    a <- takeMVar m
    putMVar m a
    return a

type Stream a = MVar (Item a)
data Item a = Item a (Stream a)

data Chan' a = Chan' (MVar (Stream a)) (MVar (Stream a))
--                    read pointer      write pointer (to an empty MVar, "the hole")


