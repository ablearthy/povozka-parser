{-# LANGUAGE DeriveFoldable #-}

module Povozka.AST where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Word (Word32)

data FullCombinatorId a
  = FullCombinatorId !a !(Maybe ByteString) !ByteString !Word32
  deriving (Foldable, Show)

data Identifier a = Identifier !a !(Maybe ByteString) !ByteString
  deriving (Foldable, Show)

data ExclMark = ExclMark deriving (Show)

newtype Expr a = Expr [Term a] deriving (Foldable, Show)

data Term a
  = TExpr !(Expr a)
  | TNumber !a !Int
  | TVar !(Identifier a)
  | TBare !a !(Term a)
  deriving (Foldable, Show)

data OptArg a = OptArg !a !(Identifier a) !(Maybe ExclMark) !(Expr a)
  deriving (Foldable, Show)

data Arg a
  = SimpleArg !(Maybe (Identifier a)) !(Maybe (Identifier a, Maybe Int)) !(Maybe ExclMark) !(Term a)
  | ArrayArg !(Maybe (Identifier a)) !(Maybe Word32) ![Arg a]
  deriving (Foldable, Show)

data ResultType a = ResultType !(Identifier a) !(Expr a) deriving (Foldable, Show)

data Decl a = Decl !(FullCombinatorId a) ![OptArg a] ![Arg a] !(ResultType a)
  deriving (Foldable, Show)

data Schema a
  = Schema
  { funcDecls :: ![Decl a]
  , constrDecls :: ![Decl a]
  } deriving (Foldable, Show)