-- Specification for a parser for the untyped lambda-calculus, to be generated
-- by the Happy parser-generator.

{
{-# LANGUAGE DeriveFoldable #-}

module Parser
    ( parseUntyped
    ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import qualified Lexer as L
import Ast
}

-- Happy settings
%name parseUntyped -- Name of generated parsing function
%expect 0 -- No intentional Shift/Reduce conflicts.
%monad { L.Alex } { >>= } { pure } -- Todo: Use custom monad instead of alex's?
%lexer { lexer } { L.RangedToken L.EOF _ }
%error { parseError }
%tokentype { L.RangedToken }

%token
    IDENT { L.RangedToken (L.Ident _) _ }
    '\\' { L.RangedToken L.Lambda _ }
    '.' { L.RangedToken L.Dot _ }
    '(' { L.RangedToken L.LPar _ }
    ')' { L.RangedToken L.RPar _ }
    

-- Begin rules section
%%

exp :: { Exp L.Range }
    : '\\' var '.' exp { EAbs (L.rtRange $1 <-> getRange $4) $2 $4 }
    | appExp { $1 }

appExp :: { Exp L.Range }
    : simpleExp { $1 }
    | appExp simpleExp { EApp (getRange $1 <-> getRange $2) $1 $2 }

simpleExp :: { Exp L.Range }
    : var { EVar (getRange $1) $1 }
    | '(' exp ')' { $2 } -- todo expand range?

var :: { Name L.Range }
    : IDENT { unTok $1 (\range (L.Ident name) -> Name range name) }


-- End Rules section, begin supporting datatypes and functions
{

parseError :: L.RangedToken -> L.Alex a
parseError _ = do
    (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
    L.alexError $ 
        "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)

unTok :: L.RangedToken -> (L.Range -> L.Token -> a) -> a
unTok (L.RangedToken tok range) ctor = ctor range tok

-- Concatenate two ranges.
(<->) :: L.Range -> L.Range -> L.Range
L.Range a1 _ <-> L.Range _ b2 = if a1 < b2 then L.Range a1 b2 
    else error $ "Invalid range concatenation " <> show a1 <> "<->" <> show b2


}
