{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}

module ReactionModel where
    import qualified Data.Map.Strict as M
    -- import Data.Text as T
    -- import Database.Bolt
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

    class CoreReactionA block result | block -> result where
        fromMolecules :: [Molecule] ->  block
        withAcceleretaion :: Catalyst -> ACCELERATE ->  block
        produce :: [Molecule] -> PRODUCT_FROM -> block
        construct :: Reaction -> [block] -> result

    data Node = Node {alias :: String, nq:: String} deriving Show
    data Relation = Relation { rq:: String } deriving Show
    data ProcessQuery = ProcessQuery {relations :: [(Node, Relation)]} deriving Show
    data CypherQuery = CypherQuery {getQuery :: String} deriving Show

    cypherAlias s = "(" ++ s ++ ")"

    instance CoreReactionA ProcessQuery CypherQuery where
        fromMolecules = ProcessQuery . fmap (\m -> (mkMol m, Relation "-[:REAGENT_IN]->")) where
             mkMol :: Molecule -> Node
             mkMol (Molecule _id sm iu) = let 
                _alias = 'm': show _id
                _q = printf "(%s:Molecule {id:%d, smiles:'%s', iupac:'%s'})" _alias _id sm iu
                in Node _alias _q
        
        withAcceleretaion (Catalyst _id sm name) acc = ProcessQuery . pure $ (node, relation) where
            nodeAlias =  'c': show _id
            _name :: String
            _name = maybe "" (printf ", name:'%s'") name
            node = Node nodeAlias (printf "(%s:Catalyst {id:%d, smiles:'%s'%s})" nodeAlias _id sm _name)
            relation = Relation $ liftA3 (printf "-[:%s {temperature:'%f', pressure:'%f'}]->") (show . typeOf) temperature pressure

        construct (Reaction _id name) blocks = let
            _reactionAlias = 'r': show _id
            _rQ = printf "MERGE (%s:Reaction {id:%d, name:'%s'})" _reactionAlias _id name
            _bloqckQ ra (node, relation) = " MERGE " ++ nq node ++ " MERGE " ++  cypherAlias (alias node) ++ rq relation ++ cypherAlias ra
            encodedBlocks = mconcat $ blocks >>= fmap (_bloqckQ _reactionAlias) . relations
            in CypherQuery (_rQ <> encodedBlocks)



    r1 = Reaction 1 "fuse"

    m1 = Molecule 1 "mol smiles 1" "mol iupac 1"
    m2 = Molecule 2 "mol smiles 2" "mol iupac 2"
    m3 = Molecule 3 "mol smiles 3" "mol iupac 3"
    
    constructV1 :: Reaction -> [ProcessQuery] -> CypherQuery
    constructV1 = construct