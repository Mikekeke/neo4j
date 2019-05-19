{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Bolt
import Data.Default (Default (..))
import Control.Exception
import Data.Text as T (unpack, append)

import Model
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
    -- -- putStrLn (T.unpack qs)
    -- pipe <- connect boltCfg
    -- let (alias, q, params) = toPreQuery m1
    -- -- run pipe (query_ qs)
    -- run pipe (uncurry queryP $ protoQ pr1 )
    -- -- mapM_ (run pipe . toQuery) [m1,m2]
    -- -- mapM_ (run pipe . toQuery) [r1]
    -- putStrLn "close"
    -- close pipe
    -- return ()
