-- Abstract Syntax Tree

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE InstanceSigs #-}

module Ast where
import Data.ByteString.Lazy.Char8 (ByteString)

data Name a
    = Name a ByteString
    deriving (Foldable, Show)

data Exp a
    = EAbs a (Name a) (Exp a)
    | EApp a (Exp a) (Exp a)
    | EVar a (Name a)
    deriving (Foldable, Show)

class Ranged a where
    getRange :: a b -> b

instance Ranged Exp where
    getRange :: Exp a -> a
    getRange (EAbs r _ _) = r
    getRange (EApp r _ _) = r
    getRange (EVar r _) = r

instance Ranged Name where
    getRange :: Name a -> a
    getRange (Name r _) = r

-- getRangeExp :: Exp a -> a
-- getRangeExp (EAbs r _ _) = r
-- getRangeExp (EApp r _ _) = r
-- getRangeExp (EVar r _) = r

-- getRangeName :: Name a -> a
-- getRangeName (Name r _) = r
