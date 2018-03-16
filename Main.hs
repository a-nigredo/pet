module Main where

main :: IO ()

main = do putStrLn "Hello, put an user id?"
          id <- getLine
          do {
            user <- (case (findById(id) :: Maybe User) of Just user -> Right user
                                                          Nothing -> Left "User not found");
            res <- validate user;
            print res;
          }

newtype Password = Password String deriving (Show, Eq)
data User = User { userId::Int, name::String, pwd::Password } deriving (Eq, Show)

class Validator a where
  validate :: a -> Either String a

class Repository a where
  findAll :: [a]
  findById :: Int -> Maybe a

instance Validator User where
  validate user = if length(name user) < 20
    then Left "Name too short"
    else Right user

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
