{-# LANGUAGE TemplateHaskell,RankNTypes #-}

import Control.Lens
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

data Game = Game
    { _score :: Int
    , _units :: [Unit]
    , _boss  :: Unit
    } deriving (Show)

data Unit = Unit
    { _health   :: Int
    , _position :: Point
    } deriving (Show)

data Point = Point
    { _x :: Double
    , _y :: Double
    } deriving (Show)

makeLenses ''Game
makeLenses ''Unit
makeLenses ''Point

initialState :: Game
initialState = Game
    { _score = 0
    , _units =
        [ Unit
            { _health = 10
            , _position = Point { _x = 3.5, _y = 7.0 }
            }
        , Unit
            { _health = 15
            , _position = Point { _x = 1.0, _y = 1.0 }
            }
        , Unit
            { _health = 8
            , _position = Point { _x = 0.0, _y = 2.1 }
            }
        ]
    , _boss = Unit
        { _health = 100
        , _position = Point { _x = 0.0, _y = 0.0 }
        }
    }

strike :: StateT Game IO ()
strike = do
    lift $ putStrLn "*shink*"
    boss.health -= 10

-- or:
bossHP :: Lens' Game Int
bossHP = boss . health

strike' :: StateT Game IO ()
strike' = do
    lift $ putStrLn "*shink*"
    bossHP -= 10

fireBreath :: StateT Game IO ()
fireBreath = do
    lift $ putStrLn "*rawr*"
    units.traversed.health -= 3

-- or:

partyHP :: Traversal' Game Int
partyHP = units.traversed.health

fireBreath2 :: StateT Game IO ()
fireBreath2 = do
    lift $ putStrLn "*rawr*"
    partyHP -= 3

-- this needs RankNTypes. probably because the Traversal' expands to some forall voodoo.
around :: Point -> Double -> Traversal' Unit Unit
around center radius = filtered (\unit ->
    (unit^.position.x - center^.x)^2
  + (unit^.position.y - center^.y)^2
  < radius^2)
  -- this "Traversal'" violates the Traversal laws, because (I think) the selected things are different
  -- depending on their value, and then using the Traversal twice can lead to different things.
  -- "filtered" is generally dangerous, but useful...

fireBreath3 :: Point -> StateT Game IO ()
fireBreath3 target = do
    lift $ putStrLn "*rawr*"
    -- units.traversed.health -= 3 -- old way
    units.traversed.(around target 1.0).health -= 3

retreat :: StateT Game IO ()
retreat = do
    lift $ putStrLn "Retreat!"
    zoom (units.traversed.position) $ do
        x += 10
        y += 10

-- or
partyLoc :: Traversal' Game Point
partyLoc = units.traversed.position

retreat2 :: StateT Game IO ()
retreat2 = do
    lift $ putStrLn "Retreat!"
    zoom partyLoc $ do
        x += 10
        y += 10

battle :: StateT Game IO ()
battle = do
    -- Charge!
    forM_ ["Take that!", "and that!", "and that!"] $ \taunt -> do
        lift $ putStrLn taunt
        strike

    -- The dragon awakes!
    fireBreath3 (Point 0.5 1.5)
    
    replicateM_ 3 $ do
        -- The better part of valor
        retreat

        -- Boss chases them
        zoom (boss.position) $ do
            x += 10
            y += 10
