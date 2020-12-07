{-# LANGUAGE InstanceSigs, ScopedTypeVariables #-}

-- continue in this file to avoid clashes
import Control.Monad.Cont

--- more general exception foo
--                        e-h, computation          error*handler
tryCont :: MonadCont m => ((err -> m a ) -> m a) -> (err -> m a) -> m a
tryCont c h = callCC $ \ok -> do
    err <- callCC $ \notOk -> do
        x <- c notOk -- this *can* jump out of the inner callCC, so ok is skipped and handler is called instead
        ok x -- this jumps out of the outer callCC, so handler is never called
    h err

data SqrtException = LessThanZero deriving (Show,Eq)

sqrtIO :: (SqrtException -> ContT r IO ()) -> ContT r IO ()
sqrtIO throw = do
    ln <- lift (putStr "Enter a number to sqrt: " >> readLn)
    when (ln < 0) (throw LessThanZero)
    lift $ print (sqrt ln)

m = runContT (tryCont sqrtIO (lift . print)) return

-- pattern matching
check :: Bool -> String
check b = case b of
            True -> "It's True"
            False -> "It's False"

type BoolCPS r = r -> r -> r

true :: BoolCPS r
true x _ = x

false :: BoolCPS r
false _ x = x

--- ^ this seems familiar.

check' :: BoolCPS String -> String
check' b = b "It's True" "It's False"

conv1 :: (Bool -> Bool -> a) -> a
conv1 = \b -> b True False

conv2 :: Bool -> BoolCPS a
conv2 = \b -> if b then true else false

data Foobar = Zero | One Int | Two Int Int deriving (Eq,Show)

type FoobarCPS r = r -> (Int -> r) -> (Int -> Int -> r) -> r

zero :: FoobarCPS r
zero x _ _ = x

one :: Int -> FoobarCPS r
one x _ f _ = f x

two :: Int -> Int -> FoobarCPS r
two x y _ _ f = f x y

fun :: Foobar -> Int
fun x = case x of
          Zero -> 0
          One a -> a + 1
          Two a b -> a + b + 2

funCPS :: FoobarCPS Int -> Int
funCPS x = x 0 (+1) (\a b -> a + b + 2)

f1 = fun Zero
f2 = fun $ One 2
f3 = fun $ Two 1 1

f4 = funCPS zero
f5 = funCPS $ one 2
f6 = funCPS $ two 1 1

-- Noteworthy: this sort of pseudo-pattern matching doesn't need Eq instance, no comparisons here.
-- This still seems a bit magical, and not necessarily all that useful ?_?




-- todo: magic coroutine example.
