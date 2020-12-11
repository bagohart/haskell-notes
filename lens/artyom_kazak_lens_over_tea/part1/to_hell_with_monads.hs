import Control.Monad
import Data.Bifunctor

-- f should be a Functor
type Lens f s a = (a -> f a) -> s -> (a, f s)

ix :: Int -> Lens [] [a] a
ix index f list
    | index < 0 = error "ix: negative index :("
    | null list = error "ix: index too large :("
    | old:rest <- list = if index == 0
                            then (old, (: rest) <$> (f old))
                            else second (fmap (old :)) $ ix (index-1) f rest

