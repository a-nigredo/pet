module Main where

main :: IO ()
main = print (findById(89) :: Maybe User)

newtype Password = Password String deriving (Show, Eq)
data User = User { userId :: Int, name::String, pwd::Password }
            deriving (Eq, Show)

class Repository a where
  findAll :: [a]
  findById :: Int -> Maybe a

instance Repository User where
  findAll = [
             User{userId = 1, name = "Andrii", pwd = Password "pwd"},
             User{userId = 2, name = "Olya", pwd = Password "pwd2"}
            ]
  findById id = (safeHead . filter(by userId id)) findAll

by :: Eq b => (a -> b) -> b -> a -> Bool
by f value record = f record == value

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x
