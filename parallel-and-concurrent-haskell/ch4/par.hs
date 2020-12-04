import Control.Monad.Par -- stack ghci --package monad-par
import Control.Monad

-- runPar :: Par a -> a
-- ^ runs the computation in the monad to get a result. this is still parallel & deterministic.

-- fork :: Par () -> Par ()
-- ^ caller of fork inputs a Par () computation, that is then executed in parallel with this caller.
-- hm.

-- To pass values between Par computations, use the IVar type:
-- data IVar a
-- ^ Must be created and used within the same Par. (is/should be enforced by the types, similar to ST/STRef)
--
-- new :: Par (IVar a)
-- ^ IVar is a box that starts empty
--
-- put :: NFData a => IVar a -> a -> Par ()
-- ^ store a value in the box.
-- can only be called ONCE. (otherwise error)
-- it calls deepseq to start evaluating immediately.
-- there is also put_ which evaluates to WHNF only.
-- This can save time if WHNF means that full evaluation has already happened.
-- If you don't understand what you're doing then... well x_X
--
-- get :: IVar a -> Par a
-- ^ read the value. if it's empty -> wait until it's not empty.
-- does NOT remove the value from the box


fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

comp1 :: Int -> Int -> Par Int
comp1 n m = do
    i <- new :: Par (IVar Int) -- let's build two boxes. The results will go in there.
    j <- new :: Par (IVar Int)
    let c1 = put i (fib n) -- this is a computation that produces fib n and stores it in the IVar i
    let c2 = put j (fib m)
    fork c1 -- actually use the defined computations to build this new one
    fork c2
    a <- get i -- wait for results from the computations that put stuff into the IVars i and j
    b <- get j
    return (a+b)

-- forks the given computation and returns the IVar with its result
spawn' :: NFData a => Par a -> Par (IVar a)
spawn' p = do
    i <- new
    let comp = do
        x <- p
        put i x
    fork comp
    return i

-- [a] ~> [Par b] ~> [Par (IVar b)] ~> Par [IVar b] ~> Par [Par b] ~> Par (Par [b]) ~> Par [b]
parMapM' :: NFData b => (a -> Par b) -> [a] -> Par [b]
parMapM' f as = do
    ivarbs <- mapM (spawn' . f) as
    mapM get ivarbs

-- this implementation seems easier than what the book suggests, which is reimplementing the whole thing
parMap' :: NFData b => (a -> b) -> [a] -> Par [b]
parMap' f = parMapM' (return . f)

-- ^ parMapM' and parMap create computations that wait for all results before returning
-- why? because of the get probably, that is applied to all the things and waits. or something.
-- this can be prevented with mapM (spawn . f) u_U

