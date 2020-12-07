{-# LANGUAGE InstanceSigs, ScopedTypeVariables #-}

import Control.Monad.Trans.Cont
import Control.Monad

add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

pythagoras :: Int -> Int -> Int
pythagoras x y = add (square x) (square y)

---

-- adds two numbers and expects further instructions to do something with the result
add_cps :: Int -> Int -> ((Int -> r) -> r)
add_cps x y = \k -> k (add x y)

square_cps :: Int -> ((Int -> r) -> r)
square_cps x = \k -> k (square x)

pythagoras_cps :: Int -> Int -> ((Int -> r) -> r)
-- pythagoras_cps x y = \k -> k (add (square x) (square y)) -- should be equivalent to this
pythagoras_cps x y = \k ->
    square_cps x $ (\x_squared ->
        square_cps y $ (\y_squared ->
            add_cps x_squared y_squared $ k))

test :: IO ()
test = pythagoras_cps 3 4 print

test' :: IO ()
test' = (flip ($)) print (pythagoras_cps 3 4)

thrice :: (a -> a) -> a -> a
thrice f x = f $ f $ f $ x

th1 = thrice (+1) 0

-- transform argument to CPS: (a -> a) ~~~~> (a -> ((a -> r) -> r))
-- transform result to CPS: a ~~~~> ((a -> r) -> r)
thrice_cps :: (a -> ((a -> r) -> r)) -> a -> ((a -> r) -> r)
--                      f               x         k
thrice_cps f_cps x = \k -> 
    f_cps x $ (\fx ->
        f_cps fx $ (\ffx ->
            f_cps ffx $ k))

th2 :: IO ()
th2 = thrice_cps (\x -> \k -> k $ x + 1) 0 print

-- combinator to apply a CPS function to a suspended computation
chainCPS :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
--          suspended comp s    CPS function f                g    new suspended comp
-- idea: s expects a function starting with a. f is such a function.
-- But then, r :: (b -> r) -> r. uh oh. Therefore, f doesn't cut it, but I can modify it, maybe...
chainCPS s f = \k -> s (\x -> f x k)

newtype Cont' r a = Cont' ((a -> r) -> r)

cont' :: ((a -> r) -> r) -> Cont' r a
cont' = Cont'

runCont' :: Cont' r a -> ((a -> r) -> r)
runCont' (Cont' arr) = arr

-- Functor and instance could be expressed using the Monad thing of course, but
-- let's do this directly...
instance Functor (Cont' r) where 
    fmap :: (a -> b) -> Cont' r a -> Cont' r b
    fmap g (Cont' arr) = Cont' $ \k -> -- k :: (b -> r)
        arr (\x -> k (g x)) -- x :: a, g x :: b

instance Applicative (Cont' r) where 
    pure x = Cont' $ \k -> k x
    (<*>) :: Cont' r (a -> b) -> Cont' r a -> Cont' r b
    (<*>) (Cont' abrr) (Cont' arr) = Cont' $ \k -> -- abrr :: ((a -> b) -> r) -> r
                abrr (\fnab -> arr (\a -> k (fnab a)))

instance Monad (Cont' r) where 
    return :: a -> Cont' r a
    return x = cont' ($ x)

    (>>=) :: Cont' r a -> (a -> Cont' r b) -> Cont' r b
    (>>=) s f = cont' $ \c -> runCont' s (\x -> runCont' (f x) c)

add_cont :: Int -> Int -> Cont' r Int
add_cont x y = return (add x y)

square_cont :: Int -> Cont' r Int
square_cont x = return (square x)

pythagoras_cont :: Int -> Int -> Cont' r Int
pythagoras_cont x y = do
    x_squared <- square_cont x
    y_squared <- square_cont y
    add_cont x_squared y_squared

-- not sure if this^ is really better than v

pythagoras_cont' :: Int -> Int -> Cont' r Int
pythagoras_cont' x y = do
    x_squared <- return (square x)
    y_squared <- return (square y)
    return (add x_squared y_squared)

-- ... let alone if any of this is useful o_O

test_cont :: IO ()
test_cont = (runCont' $ pythagoras_cont 2 2) print

-- callCC next
-- no callCC here
squareSansCC :: Int -> Cont r Int
squareSansCC n = return (n ^ 2)

-- callCC here
squareCCC :: Int -> Cont r Int
squareCCC n = callCC $ \k -> k (n ^ 2)
-- pure x = Cont' $ \k -> k x
-- ... this is... just return?

foo :: Int -> Cont r String
foo x = callCC $ \k -> do
    let y = x ^ 2 + 3
    when (y > 20) $ k "over twenty" -- this k invocation is a magical exit point. dafuq ?_?
    return (show $ y - 4)

fooAp0 = (runCont $ foo (1::Int)) (\x -> print $ "I am the continuation: " ++ x)
fooAp1 = (runCont $ foo (2::Int)) (\x -> print $ "I am the continuation: " ++ x)
fooAp2 = (runCont $ foo (5::Int)) (\x -> print $ "I am the continuation: " ++ x)

bar :: Char -> String -> Cont r Int
bar c s = do
    msg <- callCC $ \k -> do
        let s0 = c : s
        when (s0 == "hello") $ k "They say hello." -- This becomes the value of the callCC call.
        let s1 = show s0 -- what does this even do ?_? I think this is completely useless.
        return ("They appear to be saying " ++ s0)
    return (length msg)

barAp1 = (runCont $ bar 'h' "ello") (\x -> print $ "The number is: " ++ show x)
barAp2 = (runCont $ bar 'h' "elloooooooo") (\x -> print $ "The number is: " ++ show x)
barAp3 = (runCont $ bar 'h' "elloooooooooooooooooooooooo") (\x -> print $ "The number is: " ++ show x)

quux :: Cont r Int
quux = callCC $ \k -> do
    let n = 5
    k n -- quit callCC call here.
    return 25 -- this will never be evaluated.

quuxAp1 = (runCont $ quux) (print)

--      k :: ---------------
-- Note: k need not always be called, therefore the return type of k and the return type of callCC must be identical!
-- k's argument is (a -> Cont r b) because this is the next suspended computation.
-- So it must expect an a, which is what the argument to callCC sorta produces.
-- The b is whatever happens after that.
-- hm.
callCC' :: ((a -> Cont' r b) -> Cont' r a) -> Cont' r a
callCC' f = cont' $ \h -> runCont' (f (\a -> cont' $ \_ -> h a)) h
-- it seems that k is called a in this implementation.
-- this code looks pretty magic. what does the _ even throw away?_?


-- when :: Applicative f => True -> f () -> f ()
-- this doesn't compile, because the types don't match:
-- I can follow the single steps, but I'm not sure what this is about.
-- quux' :: Cont r Int
-- quux' = callCC $ \(k :: Int -> Cont r ()) -> do
--     let n = 5
--     when True $ k n -- (k n) :: Cont r (), i.e. b ~ ()
--     let x :: Cont r ()
--         x = k 25 
--     ((return 25) :: Cont r Int)
     
-- build a weird control structure, like example 1, but simpler
fun' :: Int -> String
fun' n = (`runCont` id) $ do
    str <- callCC $ \exit1 -> do
        when (n < 10) (exit1 (show n))
        n' <- callCC $ \exit2 -> do
            when (n < 20) (exit2 (n+10)) -- exit 1 level
            when (n < 30) $ do
                exit1 (show (n-100)) -- exit 2 levels
            return n -- this will never run, but it seems I need this to compile? I could also make this "undefined"
        return (show (n'+100))
    return $ "Answer: " ++ str

-- exception foo
--                         ***exception*handler**   *ok*handler*
addExcept :: Int -> Int -> (String -> Cont r Int) -> Cont r Int
addExcept x y handler = callCC $ \ok -> do
    err <- callCC $ \notOk -> do
        when (x + y == 42) $ notOk "those numbers answer too much"
        ok $ x + y
    handler err

addExceptRun :: Int -> Int -> Int
addExceptRun x y = runCont (addExcept x y error) id

addExceptRun' :: Int -> Int -> Int
addExceptRun' x y = runCont (addExcept x y (\x -> return 1337)) id

