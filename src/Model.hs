{-# LANGUAGE OverloadedStrings #-}

module Model where

import Data.Text

newtype Person = Person {name :: Text} deriving (Eq, Show)
newtype Car = Car {mark :: Text} deriving (Eq, Show)

data Driving = Driving {years :: Maybe Int} deriving (Eq, Show)

class CypherQuery a where
    cQuery :: a -> Text

instance CypherQuery Person where
    cQuery (Person name) = pack $ mappend ":Person {name:" . shows name $ "}"

instance CypherQuery Car where
    cQuery (Car mark) = pack $ mappend ":Car {mark:" . shows mark $ "}"


instance CypherQuery Driving where
    cQuery (Driving y) = pack $ ":DRIVES " ++ (maybe "" (("{years:" ++) . (++ "}") . show) y)
    

-- instance Relationship (Driving a b) where
--     rel d = (maybe "" show (years d)) ++ ":DRIVES"