import Data.IORef

ex :: IO ()
ex = do
    box <- newIORef (4 :: Int)
    readIORef box >>= print
    modifyIORef box (2*)
    readIORef box >>= print
    writeIORef box 0
    readIORef box >>= print
