-- idea 1: append not lists, but lists of lists:
type ConcatList a = [[a]]

(+|+) :: ConcatList a -> ConcatList a -> ConcatList a
(+|+) a b = a ++ b

toList :: ConcatList a -> [a]
toList = concat

-- problem: if all lists have length 1, then this doesn't make any difference.
-- rather: we want to store lists beside each other, but not actually append them.

data AppendList1 a = Append [a] [a]
-- ... no. not lists, but our own data type...

data AppendList2 a = Append (AppendList2 a) (AppendList2 a)
-- hm, actual values missing. try again...

data AppendList3 a = JustList [a] | Append (AppendList3 a) (AppendList3 a)

-- we need to reorder (a ++ b) ++ y ... into a ++ (b ++ (y ++ ...))
doAppends :: AppendList3 a -> [a]
doAppends (JustList xs) = xs
doAppends (Append (JustList xs) y) = xs ++ doAppends y
doAppends (Append (Append a b) y) = doAppends (Append a (Append b y))

