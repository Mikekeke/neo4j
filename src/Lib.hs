{-# LANGUAGE DataKinds #-}
-- :seti -XDataKinds
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

data Repr = Node | Relation 

data Mol (n :: Repr) where Mol :: String -> Mol Node
data Mol' :: Repr -> * where Mol' :: String -> Mol' Node

data Stuff = Stuff {ins :: [Mol Node]}



-- class EncNode a where
--     prt :: a -> String

-- instance  (Model DbA) where
--     -- prt (Cns x) = show x
--     prt (Cnd x) = show x