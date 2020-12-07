data T = TI Int | TS String

plus :: T -> T -> T
plus = undefined

concat :: T -> T -> T
concat = undefined
-- ^ I could implement those, but with what semantic? combine or use the first one if in doubt?

-- now with Phantom
data T' a = TI' Int | TS' String
-- ^ the a isn't actually used. hm.
-- so it doesn't have any runtime overhead

plus' :: T' Int -> T' Int -> T' Int
plus' = undefined

concat' :: T' String -> T' String -> T' String
concat' = undefined

-- How is this useful? I can still define
t :: T' String
t = TI' 5
-- and
t' :: T' Int
t' = TS' "lol"
-- which intuitively I don't want.
-- If I want this to be useful, I would want to guarantee that a t or t' cannot be created in
-- the first place.
-- I think this is what the example about the vectors is doing,
-- but it seems needlessly complicated:
data D0 = D0 deriving (Eq,Show)
data D1 = D1
data D2 = D2
data D3 = D3

data Vector n a = Vector [a] deriving (Eq,Show)

v2d :: Vector D2 Int
v2d = Vector [1,2]

v3d :: Vector D3 Int
v3d = Vector [1,2,3]

-- lol = v2d == v3d
-- yields:
-- Couldn't match type ‘D3’ with ‘D2’
--       Expected type: Vector D2 Int
--         Actual type: Vector D3 Int
--     • In the second argument of ‘(==)’, namely ‘v3d’
--       In the expression: v2d == v3d
--       In an equation for ‘lol’: lol = v2d == v3d
--    |
-- 45 | lol = v2d == v3d

-- this works
rofl = v2d == Vector [1,2,3]

-- I'm not convinced yet that this is really enormously useful.
