{
{-# LANGUAGE OverloadedStrings #-}
module Povozka.Lexer 
    ( Alex
    , AlexPosn (..)
    , alexGetInput
    , alexError
    , runAlex
    , alexMonadScan
    , Range (..)
    , RangedToken (..)
    , Token (..)
    , scanMany) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Word
import Data.Char (ord)
}

%wrapper "monadUserState-bytestring"

$identChar = [a-zA-Z0-9_]


@id_ = [a-zA-Z] $identChar*
@id = (@id_ \.)? @id_ ("#" [0-9a-fA-F]{1,8})?

@number = [0-9]+

tokens :-

<0> $white+              ;
<0> "//" .* \n?          ;
<0> "/*" (. | \n)* "*/"  ;

<0> @id { tokId }
<0> @number { tokNumber }

<0> "---" $white* "functions" $white* "---" { tok FuncHeader }
<0> "---" $white* "types" $white* "---" { tok TypesHeader }

<0> "*" { tok Asterisk }
<0> ":" { tok Colon }
<0> ";" { tok Semicolon }
<0> "(" { tok LPar }
<0> ")" { tok RPar }
<0> "[" { tok LBracket }
<0> "]" { tok RBracket }
<0> "{" { tok LBrace }
<0> "}" { tok RBrace }
<0> "<" { tok LAngle }
<0> ">" { tok RAngle }
<0> "%" { tok Percent }
<0> "+" { tok Plus }
<0> "=" { tok Equals }
<0> "?" { tok QuestionMark }
<0> "," { tok Comma }
<0> "." { tok Dot }
<0> "#" { tok Hash }
<0> "!" { tok ExclMark }
<0> "_" { tok Underscore }



{
data AlexUserState = AlexUserState
  {
  }
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState

alexEOF :: Alex RangedToken
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  pure $ RangedToken EOF (Range pos pos)

data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq, Show)

data RangedToken = RangedToken
  { rtToken :: Token
  , rtRange :: Range
  } deriving (Eq, Show)

data Token
  = Identifier !(Maybe ByteString) !ByteString
  | IdentifierFull !(Maybe ByteString) !ByteString !Word32
  | Number !Int
  | Colon
  | Semicolon
  | LPar
  | RPar
  | LBracket
  | RBracket
  | LBrace
  | RBrace
  | LAngle
  | RAngle
  | Percent
  | Plus
  | Equals
  | QuestionMark
  | Comma
  | Dot
  | Hash
  | ExclMark
  | Underscore
  | Asterisk
  | FuncHeader
  | TypesHeader
  | EOF
  deriving (Eq, Show)

mkRange :: AlexInput -> Int64 -> Range
mkRange (start, _, str, _) len = Range{start = start, stop = stop}
  where
    stop = BS.foldl' alexMove start $ BS.take len str

tokNumber :: AlexAction RangedToken
tokNumber inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Number n
    , rtRange = mkRange inp len
    }
  where
    n = case BS.readInt (BS.take len str) of
      Just (x, "") -> x
      _ -> error "illegal number" 

tokId :: AlexAction RangedToken
tokId inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = t
    , rtRange = mkRange inp len
    }
  where
    t = case parseIdFull (BS.take len str) of
      (a, b, Just c) -> IdentifierFull a b c
      (a, b, Nothing) -> Identifier a b

parseId :: ByteString -> (Maybe ByteString, ByteString)
parseId rawId = case BS.split '.' rawId of
    [r] -> (Nothing, r)
    [a, b] -> (Just a, b)
    _ -> error "illegal identifier"

parseIdFull :: ByteString -> (Maybe ByteString, ByteString, Maybe Word32)
parseIdFull rawId = case BS.split '#' rawId of
  [a, b] -> let (ns, act) = parseId a in (ns, act, Just (parseHex b))
  [a] -> let (ns, act) = parseId a in (ns, act, Nothing)
  _ -> error "illegal full identifier"
  where
    hexChar ch
      | '0' <= ch && ch <= '9' = ord ch - ord '0'
      | 'a' <= ch && ch <= 'f' = ord ch - ord 'a' + 10
      | 'A' <= ch && ch <= 'F' = ord ch - ord 'a' + 10
      | otherwise = error "not a hex number"
    parseHex :: BS.ByteString -> Word32
    parseHex = BS.foldl' (\acc ch -> 16 * acc + (fromIntegral (hexChar ch))) 0  

tok :: Token -> AlexAction RangedToken
tok ctor inp len =
  pure RangedToken
    { rtToken = ctor
    , rtRange = mkRange inp len
    }

scanMany :: ByteString -> Either String [RangedToken]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if rtToken output == EOF
        then pure [output]
        else (output :) <$> go
}