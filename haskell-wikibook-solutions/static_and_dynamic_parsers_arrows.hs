{-# LANGUAGE Arrows,InstanceSigs #-}

import Control.Arrow
import qualified Control.Category as Cat
import Data.List (union)

data Parser s a b = P (StaticParser s) (DynamicParser s a b) -- input type [s], last result type a, next result type b
data StaticParser s = SP Bool [s] -- does it accept the empty input? which symbols does it accept?
newtype DynamicParser s a b = DP ((a, [s]) -> (b, [s])) -- last result type was a, next is b, input is [s]

-- a static parser that accepts exactly a single character
spCharA :: Char -> StaticParser Char
spCharA c = SP False [c]

-- a dynamic parser that accepts exactly a single character
-- trivial implementation because all the checks are done with the static parser
-- this is so easy only because it is a single character
dpCharA :: Char -> DynamicParser Char a Char
dpCharA c = DP (\(_,_:xs) -> (c,xs))

charA :: Char -> Parser Char a Char
charA c = P (SP False [c]) (DP (\(_,_:xs) -> (c,xs)))

-- Eq constraint for "elem"
-- it seems that the DynamicParser cannot fail? but this function can:
runParser :: Eq s => Parser s a b -> a -> [s] -> Maybe (b, [s])
runParser (P (SP emp _) (DP p)) a []
  | emp = Just (p (a, [])) 
  | otherwise = Nothing
runParser (P (SP _ start) (DP p)) a input@(x:_)
  | x `elem` start = Just $ p (a, input)
  | otherwise = Nothing

-- Arrow party starts here

instance Eq s => Cat.Category (Parser s) where 
    id :: Parser s a a
    id = P (SP True []) (DP (\(b,s) -> (b,s))) -- simply derive this from the law: id = arr id

    -- (.) :: Parser s b c -> Parser s a b -> Parser s a c
    -- (P (SP empty1 start1) (DP p2)) . (P (SP empty2 start2) (DP p1)) =
    --     P (SP (empty1 && empty2) -- combined parser can accept "" only if both parsers do
    --         (if not empty1 then start1 else start1 `union` start2)) -- start2 doesn't matter, unless parser1 accepts ""
    --       (DP (p2 . p1)) -- this part is easy

    (.) :: Parser s b c -> Parser s a b -> Parser s a c
    (P (SP empty2 start2) (DP p2)) . (P (SP empty1 start1) (DP p1)) =
        P (SP (empty1 && empty2) -- combined parser can accept "" only if both parsers do
            (if not empty1 then start1 else start1 `union` start2)) -- start2 doesn't matter, unless parser1 accepts ""
          (DP (p2 . p1)) -- this part is easy

instance Eq s => Arrow (Parser s) where 
    arr :: (a -> b) -> Parser s a b
    arr f = P (SP True []) (DP (\(x,s) -> (f x,s)))

    first :: Parser s b c -> Parser s (b,d) (c,d)
    first (P sp (DP p)) = P sp (DP (\((b,d),s) ->
        let (c, s') = p (b,s)
         in ((c,d),s')))

cp1 = charA 'a' >>> charA 'b'
cp2 = charA 'a' <<< charA 'b'
-- the output of those applied on "abc" is a bit unintuitive.
-- not sure if I made a mistake somewhere.
-- why is
-- runParser cp1 () "ab" = Nothing
-- runParser cp2 () "ab" = Just ('a',[])
-- ?
-- looks like the numbers on the implementation of (.) are indeed confused.
-- If fixed: (see above)
-- then it works as expected.
