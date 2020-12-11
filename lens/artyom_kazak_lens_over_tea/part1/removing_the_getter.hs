import Data.Bifunctor

-- this is like modify, but it also returns the unmodified value,
-- so get is obtainable from fst . modify id or something
type Lens s a = (a -> a) -> s -> (a, s)

-- ix :: Int -> (a -> a) -> [a] -> (a, [a])
ix :: Int -> Lens [a] a
ix index f list
    | index < 0 = error "setIth: negative index :("
    | null list = error "setIth: index too large :("
    | old:rest <- list = if index == 0
                            then (old, f old : rest)
                            else second (old:) $ ix (index-1) f rest

-- or use id instead of undefined
getVal = fst $ ix 3 undefined [1..10]

setVal = snd $ ix 3 (const 9001) [1..10]

modVal = snd $ ix 3 (*2) [1..10]
