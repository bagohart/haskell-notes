import Control.Concurrent
import Control.Monad
import System.IO
import Text.Printf


data Logger = Logger (MVar LogCommand)

data LogCommand = Message String | Stop (MVar ())

-- provides a handle to the logging service
initLogger :: IO Logger
initLogger = do
    m <- newEmptyMVar
    let l = Logger m
    forkIO (logger l)
    return l

-- LOG THE MESSAGE
logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)
-- here, a buffered channel would probably be more helpful.
-- Let's build this... later.

-- does not return until the logging service has processed all requests
logStop :: Logger -> IO ()
logStop (Logger m) = do
    s <- newEmptyMVar
    putMVar m (Stop s)
    takeMVar s -- this ensures to wait until the logger thread has put something into this MVar.
    -- so now we have used an MVar to put an MVar into it. ... o_O


logger :: Logger -> IO ()
logger (Logger m) = loop
    where 
        loop = do
            cmd <- takeMVar m
            case cmd of
                Message msg -> do
                  putStrLn msg
                  loop
                Stop s -> do
                    putStrLn "logger: stop"
                    putMVar s ()
                    -- ^ signal to another thread
                    -- no recursive call to loop -> exit now

main :: IO ()
main = do
    l <- initLogger
    logMessage l "hello"
    logMessage l "bye"
    logStop l
