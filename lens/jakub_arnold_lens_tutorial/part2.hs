{-# LANGUAGE RankNTypes, TupleSections #-}

import Data.Functor.Identity
import Data.Functor.Const
import Control.Lens

-- type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
-- type Lens' s a = Lens s s a a

-- over :: Lens s t a b -> (a -> b) -> s -> t
-- over ln f s = runIdentity $ ln (Identity . f) s
-- -- over ln f s = runIdentity $ ln (Identity . f) s

-- view :: Lens' s a -> s -> a
-- view ln s = getConst $ ln Const s

-- set :: Lens s t a b -> b -> s -> t
-- set ln x = over ln (const x)

-- _1 :: Functor f => (a -> f c) -> (a,b) -> f (c,b)
-- _1 f (x,y) = fmap (\a -> (a,y)) (f x)

-- _1 f (x,y) = (,y) <$> f x

-- data User = User { name :: String, age :: Int } deriving Show
-- data Project = Project { owner :: User } deriving Show

-- nameLens :: Lens' User String
-- nameLens f user = fmap (\newName -> user { name = newName }) (f (name user))

-- ageLens :: Lens' User Int
-- ageLens f user = fmap (\newAge -> user { age = newAge }) (f (age user))

-- ownerLens :: Lens' Project User
-- ownerLens f project = fmap (\newOwner -> project { owner = newOwner }) (f (owner project))

-- ownerNameLens :: Lens' Project String
-- ownerNameLens = ownerLens . nameLens

-- john = User { name = "John", age = 30 }
-- p = Project { owner = john }

data User = User String [Post] deriving Show
data Post = Post String deriving Show

posts :: Lens' User [Post]
posts f (User n p) = fmap (\p' -> User n p') (f p)

title :: Lens' Post String
title f (Post t) = fmap Post (f t)

users :: [User]
users = [User "john" [Post "hello", Post "world"], User "bob" [Post "foobar"]]
