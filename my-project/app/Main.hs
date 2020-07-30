{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Control.Exception
import Data.Text.Internal (Text(..), safe, text)

transactional :: MySQLConn -> IO a -> IO a
transactional conn procedure = mask $ \restore -> do
  execute_ conn "BEGIN"
  a <- restore procedure `onException` (execute_ conn "ROLLBACK")
  execute_ conn "COMMIT"
  pure a


main :: IO ()
main = do
    someFunc

    let dtword = "akira"
    let id = 3 

    conn <- connect
        defaultConnectInfo {ciUser = "root", ciPassword = "password", ciDatabase = "test"}

    --stmt <- prepareStmt conn "delete from memos where name=(?)"
    stmt <- prepareStmt conn "update memos set comment='comment5' where id=(?)"

    transactional conn $ do
        executeStmt conn stmt [MySQLInt8 id]

    (defs, is) <- query_ conn "SELECT * FROM memos"
    mapM_ print =<< Streams.toList is
