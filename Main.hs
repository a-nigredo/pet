module Main where

main :: IO ()
main = print (findById(1) :: Maybe User)

data User = User Int String String deriving (Eq, Show)

class Repository a where
  findAll :: [a]
  findById :: Int -> Maybe a

class SafeList a where
  safeHead :: [a] -> Maybe a
  safeHead [] = Nothing
  safeHead (x:_) = Just x

instance SafeList User

instance Repository User where
  findAll = [User 1 "Andrii" "Ivanov", User 2 "Olya" "Ivanova"]
  findById id = (safeHead . filter(byId id)) findAll

byId :: Int -> User -> Bool
byId id = \x -> case x of (User uid _ _) -> uid == id
