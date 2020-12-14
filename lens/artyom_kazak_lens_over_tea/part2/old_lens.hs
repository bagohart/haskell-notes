{-# LANGUAGE ScopedTypeVariables #-}

data OldLens s a = OldLens { get :: s -> a
                            , modify :: (a -> a) -> s -> s
                           }

-- -- we don't need a and c, but because of forall we need to specify all of them.
-- (@.) :: forall a b c. OldLens b c -> OldLens a b -> OldLens a c
-- (@.) _c _b = OldLens get' modify'
--     where get' a = let b = (get _b) a
--                     in (get _c) b
--           modify' f a = let modifyB :: b -> b -- this works because ScopedTypeVariables and forall
--                             modifyB = (modify _c) f
--                         in (modify _b) modifyB a

-- now shorten this a bit

(@.) :: OldLens b c -> OldLens a b -> OldLens a c
(@.) _c _b = OldLens get' modify'
    where get' = get _c . get _b
          modify' = modify _b . modify _c
