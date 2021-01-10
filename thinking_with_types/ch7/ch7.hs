{-# LANGUAGE ExistentialQuantification, GADTs, RankNTypes, TypeApplications, ScopedTypeVariables, TypeApplications, ConstraintKinds, KindSignatures, FlexibleInstances, UndecidableInstances #-}

import Data.Typeable
import Data.Foldable
import Data.Maybe
import GHC.Exts (Constraint)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

data Any = forall a. Any a

-- equivalent:
-- data Any where 
--  Any :: a -> Any

-- let's eliminate an existential type using CPS
elimAny :: (forall a. a -> r) -> Any -> r
elimAny f (Any a) = f a
-- caller of elimAny decides r, a is determined by the type inside the Any

-- ex. 7.1-i
-- is a function of type forall a. a -> r interesting?
-- I think, this is always a constant function - the function cannot look at the value if the type can be anything.

data HasShow where 
    HasShow :: Show t => t -> HasShow
    -- this is like Any, but with a constraint.

-- because of the constraint we can actually do something with this.
instance Show HasShow where 
    -- show (HasShow s) = show s
    show = elimHasShow show
    -- ^ ex. 7.1-iii

-- ex. 7.1-ii
-- This can't work because our instance delegates to the existing instance.
    -- HasShow :: t -> HasShow
-- We get this error:
-- No instance for (Show t) arising from a use of ‘show’
--       Possible fix:
--         add (Show t) to the context of the data constructor ‘HasShow’

elimHasShow :: (forall a. Show a => a -> r) -> HasShow -> r
elimHasShow f (HasShow a) = f a

-- 7.1.1
data Dynamic where 
    Dynamic :: Typeable t => t -> Dynamic

elimDynamic :: (forall a. Typeable a => a -> r) -> Dynamic -> r
elimDynamic f (Dynamic a) = f a

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

liftD2 :: forall a b r. (Typeable a, Typeable b, Typeable r) => Dynamic -> Dynamic -> (a -> b -> r) -> Maybe Dynamic
-- liftD2 d1 d2 f = (fmap Dynamic) . f <$> fromDynamic @a d1 <*> fromDynamic @b d2
-- this looks confusing. I think this is equivalent to this:
liftD2 d1 d2 f = do
    x <- fromDynamic @a d1
    y <- fromDynamic @b d2
    pure $ Dynamic (f x y)

-- like + from Python o_O
pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b =
    fromMaybe (error "bad types for pyPlus") $ asum
        [   liftD2 @String @String a b (++)
          , liftD2 @Int @Int a b (+)
          , liftD2 @String @Int a b $ \strA intB -> strA ++ show intB
          , liftD2 @Int @String a b $ \intA strB -> show intA ++ strB
        ]

-- test this in ghci with:
-- :set -XTypeApplications
-- default (Int)

pyPlusTest1 :: Maybe Int
pyPlusTest1 = fromDynamic @Int (pyPlus (Dynamic (1::Int)) (Dynamic (2::Int)))

-- this seems pretty ugly, especially considerung the explicit @Int o_O

-- 7.1.2
-- HasShow and Dynamic are similar. Let's factor this out:
-- * should be Type but doesn't work... ?_?
data Has (c :: * -> Constraint) where 
    Has :: c t => t -> Has c

elimHas :: (forall a. c a => a -> r) -> Has c -> r
elimHas f (Has a) = f a

type HasShow2 = Has Show
type Dynamic2 = Has Typeable

isMempty :: (Monoid a, Eq a) => a -> Bool
isMempty a = a == mempty

type MonoidAndEq a = (Monoid a, Eq a)
-- ^ type synonyms cannot be partially applied, so this is rather useless...

class (Monoid a, Eq a) => MonoidEq a
instance (Monoid a, Eq a) => MonoidEq a
-- this needs FlexibleInstances and UndecidableInstances. uh oh.

testNewClassThing :: Has MonoidEq
testNewClassThing = Has [True]

testMempty :: Has MonoidEq
testMempty = Has ([] :: [Bool])
-- this is the mempty

-- 7.2
newtype ST s a = ST { unsafeRunST :: a }

-- strictness because unsafePerformIO
instance Functor (ST s) where 
    fmap f (ST a) = seq a . ST $ f a

instance Applicative (ST s) where 
    pure = ST
    ST f <*> ST a = seq f . seq a . ST $ f a

instance Monad (ST s) where 
    ST a >>= f = seq a $ f a

newtype STRef s a = STRef { unSTRef :: IORef a }

-- here the two s are the same! so this function links the things!
newSTRef :: a -> ST s (STRef s a)
newSTRef = pure . STRef . unsafePerformIO . newIORef

readSTRef :: STRef s a -> ST s a 
readSTRef = pure . unsafePerformIO . readIORef . unSTRef

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef ref = pure . unsafePerformIO . writeIORef (unSTRef ref)

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = do
    a <- readSTRef ref
    writeSTRef ref $ f a

-- THE TRICK. you can run this only if the ST does not depend on its s type parameter.
runST :: (forall s. ST s a) -> a
runST = unsafeRunST

safeExample :: ST s String
safeExample = do
    ref <- newSTRef "hello"
    modifySTRef ref (++ " world")
    readSTRef ref

-- verboten: runST (newSTRef True)
-- ST s (STRef s a)
-- vs (forall s. ST s a) -> a
-- if this unified, we would get
-- runSt :: (forall s. ST s (STRef s Bool)) -> STRef s Bool
-- so on the right, the s suddenly appears, but at this point it doesn't exist o_O

-- apparently this trick can be used to restrict existence of all kinds of things. hm.
