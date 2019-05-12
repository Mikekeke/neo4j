{-# LANGUAGE OverloadedStrings #-}

module ReactionModel where
    import qualified Data.Map.Strict as M
    import qualified Data.Text as T
    import Database.Bolt
    import Data.Monoid
    import Data.List

    data Molecule = Molecule {moleculeId :: Int, moleculeSmiles :: String, moleculeIupac :: String} deriving Show
    data Reaction = Reaction {reactionId :: Int, reactionName :: String} deriving Show
    data Catalyst = Catalyst {catalystId :: Int, catalystSmiles :: String, catalystName :: Maybe String} deriving Show
    data PRODUCT_FROM = PRODUCT_FROM {amount :: Float} deriving Show
    data ACCELERATE = ACCELERATE {temperature :: Float, pressure :: Float} deriving Show

    data ReactionModel = ReactionModel {
        reagents :: [Molecule]
        , reaction :: Reaction
        , catalysts :: [(ACCELERATE, Catalyst)]
        , products :: [(PRODUCT_FROM, Molecule)]
    } deriving Show

    m1 = Molecule 1 "mol smiles 1" "mol iupac 1"
    m2 = Molecule 2 "mol smiles 2" "mol iupac 2"
    m3 = Molecule 3 "mol smiles 3" "mol iupac 3"
    r1 = Reaction 1 "fuse"
    c1 = Catalyst 1 "cat smile 1" (Just "cat smiles 1")
    p1 = PRODUCT_FROM 2.0
    a1 = ACCELERATE 99.0 100

    rm1 = ReactionModel [m1, m2] r1 [(a1, c1)] [(p1, m3)]

    pack' :: String -> Value
    pack' s = T $ T.pack s

    fst' (v,_,_) = v
    snd' (_,v,_) = v
    trd' (_,_,v) = v

    type Alias = T.Text

    class CanPreQuery a where
        toPreQuery :: a -> (Alias, T.Text, M.Map T.Text Value)

    toQuery m = let (_, q, params) = toPreQuery m in queryP q params

    instance CanPreQuery Molecule where
        toPreQuery (Molecule _id sm iu) = (T.pack mId, q, ps) where
            txtId = show _id 
            mId = 'm':txtId
            mSm = 's':txtId
            mUi = 'u':txtId
            q = T.pack $ "MERGE (" <> mId
                <> ":Molecule {moleculeId:{" <> mId 
                <> "}, moleculeSmiles:{" <> mSm 
                <> "}, moleculeIupac:{"<> mUi <>"}})"
            ps = M.fromList $ (\(s,a) -> (T.pack s, a)) <$> [(mId, I _id), (mSm, pack' sm), (mUi, pack' iu)]

    instance CanPreQuery Reaction where
        toPreQuery (Reaction _id _name) = (T.pack rId, q, ps) where
            txtId = show _id
            rId = 'r':txtId
            rName = "rn" <> txtId
            q = T.pack $ "MERGE (" <> rId <> ":Reaction {reactionId:{" <> rId
                <> "}, reactionName:{" <> rName <> "}})"
            ps = M.fromList $ (\(s,a) -> (T.pack s, a)) <$> [(rId, I _id), (rName, pack' _name)]

    data ProtoReaction = ProtoReaction {mols :: [Molecule], r :: Reaction} deriving Show
    pr1 = ProtoReaction [m1,m2] r1

    -- fld = foldr (\(a,q,p) (a',q',p') -> ((a:a'), (q <> T.pack ", " <> q', p <> p')) ([], mempty, mempty)

    -- instance CanPreQuery ProtoReaction where
    --     toPreQuery (ProtoReaction ms r) = undefined where
    --         preMols = toPreQuery <$> ms
    --         molIds = fst' <$> preMols
    --         molQs = snd' <$> preMols
    --         molPs = trd' <$> preMols
    --         preR = toPreQuery r
    --         rAlias = fst' preR

    toNode :: T.Text -> T.Text
    toNode t = "(" <> t <> ")"

    reagent :: T.Text -> T.Text -> T.Text
    reagent m r = "MERGE " <> toNode m <> "-[:REAGENT_IN]->" <> toNode r

    protoQ (ProtoReaction ms r) = (query, params)  where
        preMols = toPreQuery <$> ms
        molAls = fst' <$> preMols
        molQs = snd' <$> preMols
        molPs = trd' <$> preMols
        (rAlias, rq, rPs) = toPreQuery r
        maToRa = fmap (uncurry reagent) $ zip molAls (repeat rAlias)
        query = T.intercalate " " $ rq : molQs <> maToRa
        params = foldr1 M.union $ rPs : molPs