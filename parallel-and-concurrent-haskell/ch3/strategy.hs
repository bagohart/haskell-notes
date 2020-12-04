import Control.Parallel.Strategies
import Control.DeepSeq

-- type Strategy a = a -> Eval a

parPair' :: Strategy (a,b)
parPair' (a,b) = do
    a' <- rpar a
    b' <- rpar b
    return (a',b')
-- "evaluate the pair in parallel to WHNF"

using' :: a -> Strategy a -> a
using' x s = runEval (s x)

f :: Int -> Int
f x = (x + 100) `div` 17

evalIt = (f 35, f 36) `using'` parPair'
-- this is nice, it separates what to compute from how
-- removing [`using'` parPair'] should retain the same result

-- now let's build up strategies...
evalPair' :: Strategy a -> Strategy b -> Strategy (a,b)
evalPair' sa sb = \(a,b) -> do
    a' <- sa a
    b' <- sb b
    return (a',b')
-- not sure how useful this is. in the last chapter it seemed that order mattered, but here the order is fixed,
-- and choosing different strategies for the single parts doesn't change the order. hm.

parPair'' :: Strategy (a,b)
parPair'' = evalPair' rpar rpar
-- note rpar has type a -> Eval a, which is... "Strategy a". (like rseq)

rdeepseq' :: NFData a => Strategy a
rdeepseq' x = rseq (force x)

-- wraps a Strategy in an rpar o_O how is this implemented?
-- rparWith :: Strategy a -> Strategy a
-- not sure what exactly this actually does ?_?

parPair3 :: Strategy a -> Strategy b -> Strategy (a,b)
parPair3 sa sb = evalPair' (rparWith sa) (rparWith sb)

fullEvalPair :: (NFData a, NFData b) => Strategy (a,b)
fullEvalPair = parPair3 rdeepseq' rdeepseq'
-- "evaluate both components of the pair in parallel up to normal form."
-- I think, computation can here continue before both values are fully computed, because the rseq
-- is wrapped in the rparWith. uh oh.

-- no evaluation happens here
r0' :: Strategy a
r0' = return

-- toy strategy to evaluate only first component of both pairs:
toy :: Strategy ((a,b),(c,d))
toy = evalPair' (evalPair' rpar r0') (evalPair' rpar r0')

-- evaluating a list in parallel
parMap1 :: (a -> b) -> [a] -> [b]
parMap1 f xs = map f xs `using` parList1 rseq

evalList1 :: Strategy a -> Strategy [a]
evalList1 _ [] = return []
evalList1 strategy (x:xs) = do
    x' <- strategy x
    xs' <- evalList1 strategy xs
    return (x' : xs)

parList1 :: Strategy a -> Strategy [a]
parList1 strategy = evalList1 (rparWith strategy)
-- can I use parList1 to create a strategy where the elements are not computed in parallel?
-- I think this is prevented because the "not-parallel" cannot "get out" once it is called by "rparWith".
-- maybe.

