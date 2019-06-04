{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}

module ReactionModel where
    import qualified Data.Map.Strict as M
    import Data.Text as T
    -- import Database.Bolt
    import Data.Monoid
    import Data.List
    import Text.Printf
    import Data.Typeable
    import Control.Applicative
    import Control.Monad
    import Control.Arrow ((***))
    import Control.Monad.State

    data Molecule = Molecule {moleculeId :: Int, moleculeSmiles :: String, moleculeIupac :: String} deriving Show
    data Reaction = Reaction {reactionId :: Int, reactionName :: String} deriving Show
    data Catalyst = Catalyst {catalystId :: Int, catalystSmiles :: String, catalystName :: Maybe String} deriving Show
    data PRODUCT_FROM = PRODUCT_FROM {amount :: Float} deriving (Show, Typeable)
    data ACCELERATE = ACCELERATE {temperature :: Float, pressure :: Float} deriving (Show, Typeable)

    data REAGENT_IN = REAGENT_IN

    data ReactionModel = ReactionModel {
        reagents    :: [Molecule]
        , reaction  :: Reaction
        , catalysts :: [(Catalyst, ACCELERATE)]
        , products  :: [(Molecule, PRODUCT_FROM)]
    } deriving Show

    m1 = Molecule 1 "mol smiles 1" "mol iupac 1"
    m2 = Molecule 2 "mol smiles 2" "mol iupac 2"
    m3 = Molecule 3 "mol smiles 3" "mol iupac 3"
    m4 = Molecule 4 "mol smiles 4" "mol iupac 4"
    r1 = Reaction 1 "fuse"
    c1 = Catalyst 1 "cat smile 1" (Just "cat smiles 1")
    c2 = Catalyst 2 "cat smile 2" (Just "cat smiles 2")
    p1 = PRODUCT_FROM 2.0
    a1 = ACCELERATE 99.0 100
    a2 = ACCELERATE 10.0 13

    rm1 = ReactionModel [m1, m2] r1 [(c1, a1), (c2, a2)] [(m3, p1), (m4, p1)]

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
        encode = plain . liftA3 (printf "-[:%s {temperature:'%f', pressure:'%f'}]->") (show . typeOf) temperature pressure

    instance CanCypher PRODUCT_FROM RelationQuery where
        encode = plain . liftA2 (printf "<-[:%s {amount:'%f'}]-") (show . typeOf) amount

    data ProtoReaction = ProtoReaction {mols :: [Molecule], r :: Reaction, c :: Catalyst} deriving Show

    -- encTup (a,b) = (encode a, encode b)

    -- protoQ (ProtoReaction ms r c) = do


    protoQ (ReactionModel ms rctn accs prods) = do
        let molQs = fmap encode ms
            reagentQ = encode REAGENT_IN
            reactionQ = encode rctn
            accCat = fmap (encode *** encode) accs
            molProd = fmap (encode *** encode) prods
        molRelations <- traverse (\a -> mkRelationA a reagentQ reactionQ) molQs
        accReactions <- traverse (\(cat, acc) -> mkRelationA cat acc reactionQ) accCat
        prodAndMol <- traverse (\(moleculeQ, productQ) -> mkRelationA moleculeQ productQ reactionQ) molProd
        return $ T.intercalate (pack " ") $ 
            merge reactionQ : fmap merge molQs 
            ++ fmap (merge . snd) accCat   
            ++ fmap (merge . snd) molProd   
            ++ fmap merge  molRelations   
            ++ fmap merge accReactions  
            ++ fmap merge prodAndMol  
    

    encodeS :: Monad m => Molecule -> StateT [Text] m (PreQuery NodeQuery)
    encodeS m = do
        let encoded = encode m
        modify (merge encoded :)
        return encoded


    encodeDump :: [(PRODUCT_FROM, Molecule)] -> StateT [Text] Maybe ()
    encodeDump ps = do
        let mols :: [Molecule]
            mols = fmap snd ps
        undefined