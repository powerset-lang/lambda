-- Specification for a lexical analyzer for the untyped lambda-calculus, to be 
-- generated by the Alex lexical-analyzer-generator.

-- Haskell code copied verbatim into the top of the output lexer
{
{-# LANGUAGE OverloadedStrings #-}
    
module Lexer 
    ( Alex
    , AlexPosn (..)
    , alexGetInput
    , alexError
    , runAlex
    , alexMonadScan
    , Range (..)
    , RangedToken (..)
    , Token (..)
    , scanMany --todo temporary
    ) where

import Control.Monad (when)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
}

-- Alex settings
%wrapper "monadUserState-bytestring"

-- Macros
-- char set macros
$digit = [0-9]
$alpha = [a-zA-Z] --todo: unicode

-- reg ex macros
@ident = ($alpha | \_) ($alpha | \_ | \' | \-)*

-- Begin lexer rules section
untyped :-

-- Everywhere: Skip whitespace
<0> $white+ ;

<0> @ident { tokIdent }
<0> \\ { tok Lambda }
<0> "." { tok Dot }
<0> "(" { tok LPar }
<0> ")" { tok RPar }

<0> "/*" { nestComment `andBegin` comment }
<0> "*/" { \_ _ -> alexError "Lex Error: Unexpected closing comment token." }
<comment> "/*" { nestComment }
<comment> "*/" { unNestComment }
<comment> . ;
<comment> \n ;

<0> "--" { begin lineComment }
<lineComment> . ;
<lineComment> \n { begin 0 }


-- End rules; More Haskell code copied verbatim, supporting functions and types
{

{- 
    With wrapper monadUserState-bytestring, the default generated signature for
    each semantic action is:
    
    -- the Int64 is the length (bytes) of the input
    type AlexAction result = AlexInput -> Int64 -> Alex result
    
    type AlexInput = 
        ( AlexPosn    -- current position,
        , Char        -- previous char
        , ByteString  -- current input string
        , Int64       -- bytes consumed so far
        )
-}

-- Type representing all the state the lexer monad carries along
data AlexUserState = AlexUserState 
    { commentNestLevel :: Int
    }

-- value representing the initial state of the AlexUserState
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
    { commentNestLevel = 0
    }

-- function Alex uses for construction of a RangedToken representing the EOF
alexEOF :: Alex RangedToken
alexEOF = do
    startCode <- alexGetStartCode
    when (startCode == comment) $
        alexError "Lex Error: Unclosed comment."
    (pos, _, _, _) <- alexGetInput
    pure RangedToken {rtToken = EOF, rtRange = (Range pos pos)}

data Range = Range 
    { start :: AlexPosn
    , stop :: AlexPosn
    } deriving (Eq, Show)

data RangedToken = RangedToken 
    { rtToken :: Token
    , rtRange :: Range
    } deriving (Eq, Show)

data Token
    = Identifier ByteString
    | Lambda
    | Dot
    | LPar
    | RPar
    | EOF
    deriving (Eq, Show)

-- advance the lexer and make a Range for the corresponding movement
mkRange :: AlexInput -> Int64 -> Range
mkRange (start, _, str, _) len = Range {start = start, stop = stop}
    where stop = BS.foldl' alexMove start $ BS.take len str

-- Lex a token of the specified type (data constructor)
tok :: Token -> AlexAction RangedToken
tok ctor inp len = pure RangedToken
    { rtToken = ctor
    , rtRange = mkRange inp len
    }

-- Lex an identifier.
tokIdent :: AlexAction RangedToken
tokIdent inp@(_, _, str, _) len = tok (Identifier $ BS.take len str) inp len

-- Temp: a small function for testing
scanMany :: ByteString -> Either String [RangedToken]
scanMany input = runAlex input go 
    where 
        go = do
            output <- alexMonadScan
            if rtToken output == EOF 
                then pure [output] 
                else (output :) <$> go

-- todo: instance MonadState Alex where
get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())


nestComment, unNestComment :: AlexAction RangedToken
nestComment input len = do
    modify $ \s -> s{commentNestLevel = commentNestLevel s + 1}
    skip input len
unNestComment input len = do
    state <- get
    let level = commentNestLevel state - 1
    put state{commentNestLevel = level}
    when (level == 0) $ 
        alexSetStartCode 0
    skip input len 



}
