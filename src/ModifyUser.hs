{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import Data.Text
import qualified Data.Text.IO as T
import Database.SQLite.Simple
import System.IO
import User

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  conn <- open "finger.db"

  putStrLn "Current users:"
  users <- query_ conn allUsers :: IO [User]
  _ <- sequence $ fmap (T.putStrLn . username) users

  putStr "Enter username to edit: "
  usernameToEdit <- T.getLine
  user <- getUser conn usernameToEdit

  modifyUser usernameToEdit user conn

  close conn

modifyUser :: Text -> Maybe User -> Connection -> IO ()
modifyUser name Nothing _ = T.putStrLn $ "Could not find user: " <> name
modifyUser _ (Just user) conn = do
  newUsername <- promptFor "username" username
  newShell <- promptFor "shell" shell
  newHomeDirectory <- promptFor "home directory" homeDirectory
  newRealName <- promptFor "real name" realName
  newPhone <- promptFor "phone number" phone
  execute conn updateUser (newUsername, newShell, newHomeDirectory, newRealName, newPhone, userId user)
    where
      promptFor fieldName getter = do
        T.putStrLn $ "Current " <> fieldName <> ": " <> getter user
        T.putStr $ "New " <> fieldName <> " (Enter to leave as is): "
        newValue <- T.getLine
        if Data.Text.null newValue then
          return $ getter user
        else
          return $ newValue
