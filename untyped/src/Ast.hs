-- Abstract Syntax Tree

{-# LANGUAGE DeriveFoldable #-}

module Ast where
import Data.ByteString (ByteString)

data Name a
    = Name a ByteString
    deriving (Foldable, Show)

data Exp a
    = EAbs a (Name a) (Exp a)
    | EApp a (Exp a) (Exp a)
    | EVar a (Name a)
    deriving (Foldable, Show)

