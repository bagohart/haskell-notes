import Control.Parallel.Strategies
import Control.DeepSeq

f :: Int -> Int
f x = x + 100

e1 :: Eval (Int,Int)
e1 = do
    a <- rpar (f 5)
    b <- rpar (f 7)
    return (a,b)
    -- returns immediately, a and b have started computing towards WHNF and still work.
    -- useful if we don't need the result(s) [next?].

e2 :: Eval (Int,Int)
e2 = do
    a <- rpar (f 5)
    b <- rseq (f 7)
    return (a,b)
    -- returns when b is evaluated to WHNF, a has begun and still works
    -- this is mostly not useful, because how do you know which computation takes longer?

e3 :: Eval (Int,Int)
e3 = do
    a <- rpar (f 5)
    b <- rseq (f 7)
    rseq a
    return (a,b)
    -- returns when both a and b are evaluated to WHNF. both computations start evaluating at almost the same time.
    -- when we need the results here to continue, this rpar/rseq/rseq thing is useful to make it explicit.

e4 :: Eval (Int,Int)
e4 = do
    a <- rpar (f 5)
    b <- rpar (f 7)
    rseq a
    rseq b
    return (a,b)
    -- this is like rpar/rseq/rseq (e3), but a bit longer and less confusing.

-- this is basically the sudoku example without the sudoku
solve :: Int -> Int
solve x = x + 100 `div` 37

tasks :: [Int]
tasks = [1..10000]

comp1 :: Eval [Int]
comp1 = do
    let (as,bs) = splitAt (length tasks `div` 2) tasks
    as' <- rpar (force (map solve as))
    bs' <- rpar (force (map solve bs))
    rseq as'
    rseq bs'
    return (as' ++ bs')
-- the limitations here are that we use always 2 cores,
-- and the partitioning is static. both is bad.

main1 :: IO ()
main1 = do
    let solutions = runEval comp1
    putStrLn $ show $ length solutions

parMap' :: (a -> b) -> [a] -> Eval [b]
parMap' f [] = return []
parMap' f (a:as) = do
    b <- rpar (f a)
    bs <- parMap' f as
    return (b:bs)
-- create a 'spark' for each task. this is basically calling rpar for solving each task. lol.
-- this creates implicit dynamic partitioning: when a core is free, it gets a spark.
-- so all the magic machinery that is desired here is already implemented.

-- coming back from the next chapter, this can also be written as
-- map solve tasks `using` parList1 rseq
-- ... ok.

comp2 :: Eval [Int]
comp2 = parMap' solve tasks

main2 :: IO ()
main2 = do
    let solutions = runEval comp2
    putStrLn $ show $ length solutions

