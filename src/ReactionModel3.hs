{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}

module ReactionModel where
    import qualified Data.Map.Strict as M
    -- import Data.Text as T
    -- import Database.Bolt
    import Data.Semigroup (Semigroup)
    import Data.Monoid
    import Data.List
    import Text.Printf
    import Data.Typeable
    import Control.Applicative
    import Control.Monad
    import Control.Arrow ((***))
    import Control.Monad.State
    import Data.Function

    data Molecule = Molecule {moleculeId :: Int, moleculeSmiles :: String, moleculeIupac :: String} deriving Show
    data Reaction = Reaction {reactionId :: Int, reactionName :: String} deriving Show
    data Catalyst = Catalyst {catalystId :: Int, catalystSmiles :: String, catalystName :: Maybe String} deriving Show
    data PRODUCT_FROM = PRODUCT_FROM {amount :: Float} deriving (Show, Typeable)
    data ACCELERATE = ACCELERATE {temperature :: Float, pressure :: Float} deriving (Show, Typeable)
    data REAGENT_IN = REAGENT deriving Show

    type Molecules = [Molecule]
    class CoreReactionA res where
        fromMolecules :: Molecules -> Reaction -> res
        withAcceleretaion :: Catalyst -> ACCELERATE ->  Reaction -> res
        produce :: Molecules -> PRODUCT_FROM -> Reaction -> res
        build :: Reaction -> [Reaction -> res] -> res

    newtype CypherQuery = CypherQuery {getQuery :: String} deriving (Show, Semigroup, Monoid)

    cypherNode s = "(" ++ s ++ ")"
    reactionAlias r =  'r' : show (reactionId r)

    instance CoreReactionA CypherQuery where
        fromMolecules ms (reactionAlias -> ra) = CypherQuery $ foldMap g ms where
            -- ProcessQuery . fmap (\m -> (mkMol m, Relation "-[:REAGENT_IN]->")) where
            g (Molecule _id sm iu) = let _alias = 'm': show _id in 
                printf " MERGE (%s:Molecule {id:%d, smiles:'%s', iupac:'%s'}) " _alias _id sm iu
                ++ " MERGE " ++ cypherNode ra ++ "-[:REAGENT_IN]->" ++ cypherNode _alias
        
        build r@(Reaction _id name) blocks = let
            _rQ = printf " MERGE (%s:Reaction {id:%d, name:'%s'}) " (reactionAlias r) _id name
            encodeBlocks = mconcat . (fmap getQuery) . sequence blocks
            in CypherQuery $ _rQ <> encodeBlocks r
            



    r1 = Reaction 1 "fuse"

    m1 = Molecule 1 "mol smiles 1" "mol iupac 1"
    m2 = Molecule 2 "mol smiles 2" "mol iupac 2"
    m3 = Molecule 3 "mol smiles 3" "mol iupac 3"
    
    mkCypher :: CypherQuery -> CypherQuery
    mkCypher = id
    constructV1 :: CoreReactionA a => (a -> a) -> a
    constructV1 f  = f $ build r1 [fromMolecules [m1,m2]]

    constructV2 :: CoreReactionA a => a
    constructV2 = construct r1 [fromMolecules [m1,m2]]