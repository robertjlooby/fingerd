module Main where

import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import System.IO
import User

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  username <- promptFor "Username"
  shell <- promptFor "Shell"
  home <- promptFor "Home directory"
  realName <- promptFor "Real name"
  phone <- promptFor "Phone number"
  conn <- open "finger.db"
  execute conn insertUser (Null, username, shell, home, realName, phone)
  close conn
    where
      promptFor msg = do
        putStr $ msg ++ ": "
        getLine
