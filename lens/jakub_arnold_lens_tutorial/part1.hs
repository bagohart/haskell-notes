{-# LANGUAGE RankNTypes, TupleSections #-}

import Data.Functor.Identity
import Data.Functor.Const

-- project.owner.name = newThing
setOwnerName :: String -> Project -> Project
setOwnerName newName p = p { owner = (owner p) { name = newName } }

-- the naive getter/setter pair fails because what to with side effects? aka
-- overIO :: (a -> IO a) -> s -> IO s

type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

over :: Lens s a -> (a -> a) -> s -> s
over ln f s = runIdentity $ ln (Identity . f) s
-- over ln f s = runIdentity $ ln (Identity . f) s

view :: Lens s a -> s -> a
view ln s = getConst $ ln Const s

set :: Lens s a -> a -> s -> s
set ln x = over ln (const x)

_1 :: Functor f => (a -> f a) -> (a,b) -> f (a,b)
_1 f (x,y) = fmap (\a -> (a,y)) (f x)
-- _1 f (x,y) = (,y) <$> f x

data User = User { name :: String, age :: Int } deriving Show
data Project = Project { owner :: User } deriving Show

nameLens :: Lens User String
nameLens f user = fmap (\newName -> user { name = newName }) (f (name user))

ageLens :: Lens User Int
ageLens f user = fmap (\newAge -> user { age = newAge }) (f (age user))

ownerLens :: Lens Project User
ownerLens f project = fmap (\newOwner -> project { owner = newOwner }) (f (owner project))

ownerNameLens :: Lens Project String
ownerNameLens = ownerLens . nameLens

john = User { name = "John", age = 30 }
p = Project { owner = john }
