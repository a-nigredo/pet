module Main where

import Control.Monad.Trans.Either

main :: IO ()

main = do
      res <- runST $ do {
        id <- lift getLine;
        user <- (case findById(read id) of Nothing -> Left "User not found"
                                           Just user -> Right user);
        validation <- lift validate(user);
        (case validation of Left error -> lift $ putStrLn error
                            Right user -> lift $ putStrLn(show user));
      };
      (case res of Left n -> putStrLn $ show n
                   Right u -> putStrLn $ show u)

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
