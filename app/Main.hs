{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Bolt
import Data.Default (Default (..))
import Control.Exception
import Data.Text as T (unpack, append, Text)

import ReactionModel


boltCfg :: BoltCfg
boltCfg = def {user = "neo4j", password = "15197"}

-- car = Car "Ford"
-- p1 = Person "Bill"
-- dr = Driving (Just 5)
-- qs = "MATCH (car" `append` cQuery car `append` ")"
--     `append` " CREATE"
--     `append` " (" `append` cQuery p1 `append` ")"
--     `append` " -[" `append` cQuery dr `append` "]->"
--     `append` " (car)"

-- withRresource ??


main :: IO ()
main = do
    putStrLn "connect"
    pipe <- connect boltCfg
    let r = (traverse query_ (reqctionToQuery rm1))
    let r2 = (traverse query_ (Nothing :: Maybe T.Text))
    -- let r = undefined
    run pipe r
    run pipe r2
    putStrLn "close"
    close pipe
    return ()
