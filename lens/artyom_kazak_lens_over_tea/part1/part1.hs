-- getter + setter = lens
data Lens s a = Lens {
                        getter :: s -> a
                     ,  setter :: a -> s -> s
                     }

setIth :: Int -> a -> [a] -> [a]
setIth index new list
    | index < 0 = error "setIth: negative index :("
    | null list = error "setIth: index too large :("
    | old : rest <- list = if index == 0
                              then new : rest
                              else old : setIth (index-1) new rest

ix :: Int -> Lens [a] a
ix i = Lens { getter = (!! i), setter = setIth i }
