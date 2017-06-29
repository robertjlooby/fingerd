module Main where

import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import System.IO
import User

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Username: "
  username <- getLine
  putStr "Shell: "
  shell <- getLine
  putStr "Home directory: "
  home <- getLine
  putStr "Real name: "
  realName <- getLine
  putStr "Phone number: "
  phone <- getLine
  conn <- open "finger.db"
  execute conn insertUser (Null, username, shell, home, realName, phone)
  close conn
