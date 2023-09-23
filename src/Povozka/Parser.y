{
{-# LANGUAGE DeriveFoldable, OverloadedStrings #-}
module Povozka.Parser (parseTL) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))
import Control.Monad (join)

import qualified Povozka.Lexer as L
import qualified Povozka.AST as A
}

%name parseTL schema
%tokentype { L.RangedToken }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.RangedToken L.EOF _ }

%token
    identifier      { L.RangedToken (L.Identifier _ _) _ }
    identifier_full { L.RangedToken (L.IdentifierFull _ _ _) _ }
    number          { L.RangedToken (L.Number _) _ }
    ':'             { L.RangedToken L.Colon _ }
    ';'             { L.RangedToken L.Semicolon _ }
    '('             { L.RangedToken L.LPar _ }
    ')'             { L.RangedToken L.RPar _ }
    '['             { L.RangedToken L.LBracket _ }
    ']'             { L.RangedToken L.RBracket _ }
    '{'             { L.RangedToken L.LBrace _ }
    '}'             { L.RangedToken L.RBrace _ }
    '<'             { L.RangedToken L.LAngle _ }
    '>'             { L.RangedToken L.RAngle _ }
    '%'             { L.RangedToken L.Percent _ }
    '+'             { L.RangedToken L.Plus _ }
    '='             { L.RangedToken L.Equals _ }
    '?'             { L.RangedToken L.QuestionMark _ }
    '.'             { L.RangedToken L.Dot _ }
    ','             { L.RangedToken L.Comma _ }
    '#'             { L.RangedToken L.Hash _ }
    '_'             { L.RangedToken L.Underscore _ }
    '!'             { L.RangedToken L.ExclMark _ }
    '*'             { L.RangedToken L.Asterisk _ }
    func_header     { L.RangedToken L.FuncHeader _ }
    types_header    { L.RangedToken L.TypesHeader _ }

%%

many_rev(p)
  :               { [] }
  | many_rev(p) p { $2 : $1 }

many(p)
  : many_rev(p) { reverse $1 }

optional(p)
  :   { Nothing }
  | p { Just $1 }

some_rev(p)
  : p { [$1] }
  | some_rev(p) p { $2 : $1 }

some(p)
  : some_rev(p) { reverse $1 }

ignore1(a, b)
  : a b { $2 }

ignore2(a, b)
  : a b { $1 }

var_ident :: { A.Identifier L.Range }
  : identifier { unTok $1 (\range (L.Identifier ns name) -> A.Identifier range ns name)}

var_ident_opt :: { Maybe (A.Identifier L.Range) }
  : var_ident { Just $1 }
  | '_' { Nothing }

type_ident :: { A.Identifier L.Range }
  : var_ident { $1 }
  | '#' { unTok $1 (\range _ -> A.Identifier range Nothing "nat")}

type_ident_opt :: { Maybe (A.Identifier L.Range) }
  : type_ident { Just $1 }
  | '_' { Nothing }

expr :: { A.Expr L.Range }
  : many(term) { A.Expr $1 }

colon_expr :: { A.Expr L.Range }
  : ',' expr { $2 }

angle_bracket_expr :: { [A.Term L.Range ] }
  : '<' expr many(colon_expr) '>' { ((A.TExpr $2):(map A.TExpr $3)) }

term :: { A.Term L.Range }
  : '(' expr ')' { A.TExpr $2 }
  {- я пока это вырублю, потому что надо уметь отличать число от типа, а мне лень этим заниматься, да и нигде это поебень не используется -}
  -- | number { unTok $1 (\r (L.Number n) -> A.TNumber r (fromIntegral n)) }
  | type_ident angle_bracket_expr { A.TExpr (A.Expr ((A.TVar $1):$2)) }
  | type_ident { A.TVar $1 }
  | '%' term { unTok $1 (\r _ -> A.TBare r $2) }


opt_args ::  { [A.OptArg L.Range] }
  : '{' some(type_ident) ':' optional('!') expr '}' { map (\i@(A.Identifier r _ _) -> A.OptArg r i (exclMarkToAST $4) $5) $2 }


term1 :: { A.Term L.Range }
  : '(' expr ')' { A.TExpr $2 }
  -- | number { unTok $1 (\r (L.Number n) -> A.TNumber r (fromIntegral n)) }
  | '%' term { unTok $1 (\r _ -> A.TBare r $2) }
  | '#' { unTok $1 (\range _ -> A.TVar (A.Identifier range Nothing "nat"))}
  | '#' angle_bracket_expr { unTok $1 (\range _ -> A.TExpr $ A.Expr ((A.TVar (A.Identifier range Nothing "nat")):$2))}


conditional_def1 :: { Maybe Int }
  : {- empty -} { Nothing }
  | '.' number { unTok $2 (\_ (L.Number n) -> Just (fromIntegral n))}

args :: { [A.Arg L.Range] }
  {- это пиздец, но я не знаю, как это пофиксить
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
   суть в том, что если записать правило так:
     args ::= var-ident-opt ':' [ conditional-def ] [ '!' ] type-term
     conditional-def ::= var-ident [ '.' nat-const ] '?'

  то будет shift/reduce conflict: пусть мы находимся после ':', тогда,
  если следующий токен --- это var_ident, то непонятно, он относится к type-term или к conditional-def,
  поэтому и вводится term1 без type-ident (который содержит var-ident)

   -}
  : var_ident_opt ':' term1 { [A.SimpleArg $1 Nothing Nothing $3]}
  | var_ident_opt ':' '!' term { [A.SimpleArg $1 Nothing (Just A.ExclMark) $4] }
  | var_ident_opt ':' var_ident conditional_def1 '?' optional('!') term  { [A.SimpleArg $1 (Just ($3, $4)) (exclMarkToAST $6) $7 ]}
  | var_ident_opt ':' var_ident { [A.SimpleArg $1 Nothing Nothing (A.TVar $3)]}
  | var_ident_opt ':' var_ident angle_bracket_expr { [A.SimpleArg $1 Nothing Nothing (A.TExpr $ A.Expr ((A.TVar $3):$4))]}
  | optional(ignore2(var_ident_opt, ':')) optional(ignore2(number, '*')) '[' some(args) ']' { [A.ArrayArg (join $1) (fmap (flip unTok (\_ (L.Number n) -> fromIntegral n)) $2) (join $4) ]}
  | '(' some(type_ident_opt) ':' optional('!') term ')' { map (\i -> A.SimpleArg i Nothing (exclMarkToAST $4) $5) $2 }
  {- тоже вводит S/R конфликт -}
  -- | optional('!') term1 { [A.SimpleArg Nothing Nothing (exclMarkToAST $1) $2] } 

result_type :: { A.ResultType L.Range }
  : type_ident expr { A.ResultType $1 $2 }
  | type_ident '<' term many(ignore1(',', term)) '>' { A.ResultType $1 (A.Expr ($3:$4)) }


decl :: { A.Decl L.Range }
  : identifier_full many(opt_args) many(args) '=' result_type ';' { A.Decl (unTok $1 (\r (L.IdentifierFull ns name i) -> A.FullCombinatorId r ns name i)) (join $2) (join $3) $5 }

schema1 :: { A.Schema L.Range }
  : func_header many(decl) {A.Schema $2 [] }
  | types_header many(decl) { A.Schema [] $2 }

schema :: { A.Schema L.Range }
  : many(decl) many(schema1) { foldr mergeSchema (A.Schema [] []) ((A.Schema [] $1):$2) } 

{
parseError :: L.RangedToken -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)

unTok :: L.RangedToken -> (L.Range -> L.Token -> a) -> a
unTok (L.RangedToken b a) f = f a b 

exclMarkToAST :: Maybe a -> Maybe A.ExclMark
exclMarkToAST (Just _) = Just A.ExclMark
exclMarkToAST Nothing = Nothing

mergeSchema :: A.Schema a -> A.Schema a -> A.Schema a
mergeSchema (A.Schema a b) (A.Schema a' b') = A.Schema (a <> a') (b <> b')

}