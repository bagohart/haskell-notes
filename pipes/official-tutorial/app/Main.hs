module Main where

import Lib

import Control.Monad (unless, forever)
import Pipes
import System.IO (isEOF)
import qualified Pipes.Prelude as P

import Control.Exception (try, throwIO)
import qualified GHC.IO.Exception as G

import Control.Applicative ((<$))
import System.IO

import Control.Monad (replicateM_)
import Prelude hiding (take)

-- Producers

stdinLn :: Producer String IO ()
stdinLn = do
    eof <- lift isEOF
    unless eof $ do
        str <- lift getLine
        yield str
        stdinLn

loop :: Effect IO ()
loop = for stdinLn $ \str -> do
    lift $ putStrLn str

loop' :: Effect IO ()
loop' = do
    eof <- lift isEOF
    unless eof $ do
        str <- lift getLine
        (lift . putStrLn) str
        loop'

listProducer :: Producer Int IO ()
listProducer = each [1..4]

listEffect = for listProducer (lift.print)

maybeProducer :: Producer Int IO ()
maybeProducer = each (Just 1)

maybeEffect = for maybeProducer (lift.print)

-- composability
duplicate :: (Monad m) => a -> Producer a m ()
duplicate x = do
    yield x
    yield x

loop2 :: Producer String IO ()
loop2 = for P.stdinLn duplicate

list2Producer = for listProducer duplicate

loop2Effect = for loop2 (lift.print)
list2Effect = for list2Producer (lift.print)

theArrowThing  = runEffect $ for P.stdinLn (duplicate ~> lift . putStrLn)
theArrowThing2 = runEffect $ for listProducer (duplicate ~> lift . putStrLn . show)

-- Consumers
stdoutLn :: Consumer String IO ()
stdoutLn = do
    str <- await
    x <- lift $ try $ putStrLn str
    case x of
      Left e@(G.IOError { G.ioe_type = t}) -> lift $ unless (t == G.ResourceVanished) $ throwIO e
      Right () -> stdoutLn

-- repeatedly feed the same input using `feed`:
theFeed = runEffect $ lift getLine >~ stdoutLn

doubleUp :: (Monad m) => Consumer String m String
doubleUp = do
    str1 <- await
    str2 <- await
    pure $ str1 <> str2

theFeed2 = runEffect $ lift getLine >~ doubleUp >~ stdoutLn

-- Pipes
pipe1 :: IO ()
pipe1 = runEffect $ P.stdinLn >-> P.stdoutLn

main1 :: IO ()
main1 = do
    hSetBuffering stdout NoBuffering
    str <- runEffect $
        ("End of input!" <$ P.stdinLn) >-> ("Broken pipe!" <$ P.stdoutLn)
    hPutStrLn stderr str

take :: Int -> Pipe a a IO ()
take n = do
    replicateM_ n $ do
        x <- await
        yield x
    lift $ putStrLn "You shall not pass!"

maxInput :: Int -> Producer String IO ()
maxInput n = P.stdinLn >-> take n

testMaxInput = runEffect $ maxInput 3 >-> P.stdoutLn

maxOutput :: Int -> Consumer String IO ()
maxOutput n = take n >-> P.stdoutLn

testMaxOutput = runEffect $ P.stdinLn >-> maxOutput 3

cat :: (Monad m) => Pipe a a m r
cat = forever $ do
    x <- await
    yield x

head2 :: (Monad m) => Int -> Pipe a a m ()
head2 = P.take

yes :: (Monad m) => Producer String m r
yes = forever $ yield "y"

theUnixThingz :: IO ()
theUnixThingz = runEffect $ yes >-> head2 3 >-> P.stdoutLn

-- ListT
-- ... o_O

-- bla

main :: IO ()
main = someFunc

