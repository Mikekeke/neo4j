{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ReactionModel where
    import qualified Data.Map.Strict as M
    import Data.Text as T
    -- import Database.Bolt
    import Data.Monoid
    import Data.List
    import Text.Printf
    import Data.Typeable
    import Control.Applicative

    data Molecule = Molecule {moleculeId :: Int, moleculeSmiles :: String, moleculeIupac :: String} deriving Show
    data Reaction = Reaction {reactionId :: Int, reactionName :: String} deriving Show
    data Catalyst = Catalyst {catalystId :: Int, catalystSmiles :: String, catalystName :: Maybe String} deriving Show
    data PRODUCT_FROM = PRODUCT_FROM {amount :: Float} deriving Show
    data ACCELERATE = ACCELERATE {temperature :: Float, pressure :: Float} deriving (Show, Typeable)

    data REAGENT_IN = REAGENT_IN

    data ReactionModel = ReactionModel {
        reagents    :: [Molecule]
        , reaction  :: Reaction
        , catalysts :: [(ACCELERATE, Catalyst)]
        , products  :: [(PRODUCT_FROM, Molecule)]
    } deriving Show

    m1 = Molecule 1 "mol smiles 1" "mol iupac 1"
    m2 = Molecule 2 "mol smiles 2" "mol iupac 2"
    m3 = Molecule 3 "mol smiles 3" "mol iupac 3"
    r1 = Reaction 1 "fuse"
    c1 = Catalyst 1 "cat smile 1" (Just "cat smiles 1")
    p1 = PRODUCT_FROM 2.0
    a1 = ACCELERATE 99.0 100

    rm1 = ReactionModel [m1, m2] r1 [(a1, c1)] [(p1, m3)]

    data CQType = NodeQuery | RelationQuery | Binded

    data PreQuery (a :: CQType) where PreQuery :: {alias :: Maybe Alias, query :: Query} -> PreQuery a

    instance Show (PreQuery a) where
        show (PreQuery a q) = printf "PreQuery {alias :: %s, query :: %s}" (show $ T.unpack <$> a) (T.unpack q)

    aliased = PreQuery . Just . T.pack
    plain = PreQuery Nothing . T.pack

    aliasToNode :: PreQuery NodeQuery -> Maybe Text
    aliasToNode = fmap (\x -> '(' `cons` x <> ")") . alias

    merge :: PreQuery a -> Text
    merge = mappend "MERGE " . query
    mkRelation :: PreQuery NodeQuery -> PreQuery RelationQuery -> PreQuery NodeQuery -> Text
    mkRelation a r b = undefined
    mkRelationA :: PreQuery NodeQuery -> PreQuery RelationQuery -> PreQuery NodeQuery -> Maybe (PreQuery Binded)
    mkRelationA a r b = do 
        a1 <- aliasToNode a
        a2 <- aliasToNode b
        return $ PreQuery Nothing (a1 <> query r <> a2)

    
    type Alias = Text
    type Query = Text

    class CanCypher a b | a -> b where
        encode :: a -> PreQuery b

    instance CanCypher Molecule NodeQuery where
        encode (Molecule _id sm iu) =  aliased alias query where 
            alias = 'm': show _id
            query = T.pack $ printf "(%s:Molecule {id:%d, smiles:'%s', iupac:'%s'})" alias _id sm iu

    instance CanCypher Reaction NodeQuery where
        encode (Reaction _id name) = aliased alias query where 
            alias = 'r':show _id 
            query = T.pack $ printf "(%s:Reaction {id:%d, name:'%s'})" alias _id name

    instance CanCypher Catalyst NodeQuery where
        encode (Catalyst _id sm name) = aliased alias query where 
            alias = 'c':show _id
            _name :: String
            _name = maybe "" (printf ", name:'%s'") name
            query = T.pack $ printf "(%s:Catalyst {id:%d, smiles:'%s'%s})" alias _id sm _name

    instance CanCypher REAGENT_IN RelationQuery where
        encode = const $ plain "-[:REAGENT_IN]->"

    instance CanCypher ACCELERATE RelationQuery where
        encode = plain . liftA3 (printf "-[:'%s' {temperature:'%f', pressure:'%f'}]->") (show . typeOf) temperature pressure

    data ProtoReaction = ProtoReaction {mols :: [Molecule], r :: Reaction, c :: Catalyst} deriving Show
    pr1 = ProtoReaction [m1,m2] r1 c1

    
    protoQ (ProtoReaction ms r c) = do
        let molQs = fmap encode ms
            reagentQ = encode REAGENT_IN
            reactionQ = encode r
            catalInQ = encode c
        molRelations <- traverse (\a -> mkRelationA a reagentQ reactionQ) molQs
        accRelation  <- mkRelationA catalInQ (encode a1) reactionQ
        -- return $ fmap merge relations ++ fmap merge molQs ++ [merge reactionQ]
        return $ T.intercalate (pack " ") $ 
            merge reactionQ : merge catalInQ : fmap merge molQs ++ fmap merge (accRelation : molRelations)       