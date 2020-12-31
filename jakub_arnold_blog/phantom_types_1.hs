-- would have to make this constructor private, and only export functionthingys.
data Message a = Message String deriving Show

data Encrypted
data PlainText

type Recipient = String

send :: Message Encrypted -> Recipient -> IO ()
send = undefined

encrypt :: Message PlainText -> Message Encrypted
encrypt = undefined

decrypt :: Message Encrypted -> Message PlainText
decrypt = undefined

newMessage :: String -> Message PlainText
newMessage = Message

-- now this is rejected
-- x = send (newMessage "hello!") "john@example.com"

-- notes:
-- 1. we could achieve the same things using newtypes, but then the two types are not related any more.
-- If we had functions that work on both types, then this would be bad.
-- Also this could be typeclassed somehow (?) but then this gets unnecessarily complicated real quick...
-- 2. Could use GADTs: different constructors initialize the phantom type differently.
