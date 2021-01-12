{-# LANGUAGE FlexibleContexts #-}

import Data.Monoid
import Data.Foldable
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class

fb1 :: IO ()
fb1 = forM_ [1..100] $ \i ->
        if i `mod` 3 == 0 && i `mod` 5 == 0 then 
            putStrLn "FizzBuzz"
        else if i `mod` 3 == 0 then 
            putStrLn "Fizz"
        else if i `mod` 5 == 0 then 
            putStrLn "Buzz"
        else 
            print i

-- run this in ghci with: runStateT fb2 ""
fb2 :: (MonadIO m, MonadState String m) => m ()
fb2 = forM_ [1..100] $ \i -> do
    put ""
    when (i `mod` 3 == 0) (modify (++"Fizz"))
    when (i `mod` 5 == 0) (modify (++"Buzz"))
    when (i `mod` 7 == 0) (modify (++"Quux"))
    str <- get
    when (null str) (put (show i))
    get >>= liftIO . putStrLn

fb3 :: Integer -> String
fb3 i =
    if i `mod` 3 == 0 && i `mod` 5 == 0 then 
        "FizzBuzz"
    else if i `mod` 3 == 0 then 
        "Fizz"
    else if i `mod` 5 == 0 then 
        "Buzz"
    else 
        show i

fb4 :: Integer -> String
fb4 i
  | i `mod` 3 == 0 && i `mod` 5 == 0 = "FizzBuzz"
  | i `mod` 3 == 0 = "Fizz"
  | i `mod` 5 == 0 = "Buzz"
  | otherwise = show i

fb5 :: Integer -> String
fb5 i
  | i `mod` 3 == 0 && i `mod` 5 == 0 = "Fizz" ++ "Buzz"
  | i `mod` 3 == 0 = "Fizz" ++ ""
  | i `mod` 5 == 0 = "" ++ "Buzz"
  | otherwise = show i
-- ... this does look a bit like a Monoid, but the default value doesn't really match the pattern... ?_?
-- so we need to handle that differently

type FizzRule = Integer -> Maybe String

rule :: Integer -> String -> FizzRule
rule n m = \i -> if i `mod` n == 0 then Just m else Nothing

fizz = rule 3 "Fizz"
buzz = rule 5 "Buzz"
quux = rule 7 "Quux"

combineRules :: [Integer -> Maybe String] -> Integer -> Maybe String
combineRules rules = \i -> mconcat <$> sequenceA ((sequenceA rules) i)
-- this isn't so helpful, because one Nothing is enough to cancel the whole thing when sequencing.

fb6 :: [FizzRule] -> [Integer] -> [String]
fb6 rules = map f
    where
        f i = fromMaybe (show i) (ruleSet i)
        ruleSet = fold rules -- this is 3 Monoids in one: (Integer -> Maybe String), Maybe String and String 

fb7 :: (Functor f, Foldable t) => t (Integer -> Maybe String) -> f Integer -> f String
fb7 rules = fmap (fromMaybe <$> show <*> fold rules) -- functions are Applicatives. yay.

-- nice idea, but how to put this into a complete program? the thing mapping a whole list didn't seem sensible.
-- Maybe like this:
main :: IO ()
main = forM_ [1..105] (putStrLn . fb)
-- going to 105 gives me the first FizzBuzzQuux (:

fb :: Integer -> String
fb = fromMaybe <$> show <*> fold [fizz,buzz,quux]
    
-- can I improve the "rule" function?
rule' :: Integer -> String -> FizzRule
rule' n str = \i -> do
    guard $ i `mod` n == 0
    return str
-- hm. debatable :)
