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
%monad { L.Alex } { >>= } { Pure }
%lexer { lexer } { L.RangedToken L.EOF _ }
%error { parseError }
%tokentype { L.RangedToken }

%token
    IDENT { L.RangedToken (L.Ident _) _ }
    LAMBDA { L.RangedToken L.Lambda _ }
    DOT { L.RangedToken L.Dot _ }
    LPAR { L.RangedToken L.LPar _ }
    RPAR { L.RangedToken L.RPar _ }
    

-- Begin rules section
%%

exp :: { Exp L.Range }
    : LAMBDA var DOT exp { EAbs $2 $4 } --todo
    | appExp { $1 }

-- Happens first, so higher precedence
appExp :: { Exp L.Range }
    : simpleExp { $1 }
    | appExp simpleExp { EApp (info $1 <-> info $2) $1 $2 }

simpleExp :: { Exp L.Range }
    : var { $1 }
    | '(' exp ')' {  }

var :: { Name L.Range }
    : IDENT { unTok $1 (\range (L.Ident name) -> Name range name) }

emptyParens : LPAR RPAR ;
emptyString : ;

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

-- Todo: make it a typeclass that is implemented for the AST and extracts field
-- Unsafe extraction of metadata
info :: Foldable f => f a -> a
info = fromJust . getFirst . foldMap pure

-- Concatenate two ranges. 
-- Todo: LHS range must start before RHS range! Add check.
(<->) :: L.Range -> L.Range -> L.Range
L.Range a1 _ <-> L.Range _ a2 = L.Range a1 b2




}
