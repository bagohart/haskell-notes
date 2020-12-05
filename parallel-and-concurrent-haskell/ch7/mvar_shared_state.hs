import Control.Concurrent
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (lookup)

type Name        = String
type PhoneNumber = String
type PhoneBook   = Map Name PhoneNumber

newtype PhoneBookState = PhoneBookState (MVar PhoneBook)

new :: IO PhoneBookState
new = do
    m <- newMVar Map.empty
    return $ PhoneBookState m
-- this always succeeds and never waits because it builds a new MVar

insert :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insert (PhoneBookState m) name number = do
    -- this blocks until the MVar has some content, i.e. it must have been created and is not currently used.
    book <- takeMVar m
    putMVar m (Map.insert name number book)
    -- lazy evaluation means that we put something in the MVar which is not yet evaluated to even WHNF!
    -- => we can release the lock immediately, but we can build up a giant chunk of insert operations
    -- that have not been evaluated yet o_O
-- There are two ways around this:
-- 1. Use "putMVar m $! Map.insert name number book"
--      ^ this forces evaluation of the argument (not sure to what form)
-- 2. To avoid both long locking and space leaks, do:
--      let book' = Map.insert name number book
--      putMVar m book'
--      seq book' (return ())
--  ^ not sure what this means if control is put to another thread immediately after releasing the lock.
--  probably the seq would just become a no-op?


lookup :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookup (PhoneBookState m) name = do
    book <- takeMVar m
    let number = Map.lookup name book -- the book example puts this line later and immediately returns the MVar
    putMVar m book -- which is probably better because then it doesn't block
    return number -- although we have lazy evaluation, so this doesn't work like that anyway probably o_O
    -- this means the value "book" is not a reference to the thing in the MVar any more,
    -- but an immutable value. So, some sort of copying must happen here in the compiler ?_?

main1 :: IO ()
main1 = do
    s <- new
    sequence_ [ insert s ("name" ++ show n) (show n) | n <- [1..10000] ]
    lookup s "name999" >>= print -- this is our own lookup function.
    lookup s "unknown" >>= print
-- the book claims that we just created mutable state.
-- I'm not convinced that's true. It seems still immutable, only sometimes the whole thing changes :)
