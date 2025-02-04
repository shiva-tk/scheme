{-# LANGUAGE OverloadedStrings #-}

module LispVal where

import qualified Data.Map as Map
import qualified Data.Text as T

import Control.Monad.Except
import Control.Monad.Reader

type Env = Map.Map T.Text LispVal

type Eval a = ReaderT Env IO a

type Func = [LispVal] -> Eval LispVal

data LispVal = Atom T.Text
             | List [LispVal]
             | Number Integer
             | String T.Text
             | Func Func
             | Lambda Func Env
             | Nil
             | Bool Bool

instance Eq LispVal where
  (Atom a)     == (Atom b)     = a == b
  (List a)     == (List b)     = a == b
  (Number a)   == (Number b)   = a == b
  (String a)   == (String b)   = a == b
  (Bool a)     == (Bool b)     = a == b
  Nil          == Nil          = True

  -- Functions cannot be compared
  (Func _)     == (Func _)     = error "Cannot compare LispVal functions for equality"
  (Lambda _ _) == (Lambda _ _) = error "Cannot compare LispVal lambda functions for equality"

  -- Everything else is false
  _            == _            = False

instance Show LispVal where
  show = T.unpack . showVal
    where
      showVal :: LispVal -> T.Text
      showVal (Atom x)     = x
      showVal (String s)   = s
      showVal (Number n)   = T.pack (show n)
      showVal (Bool True)  = "#t"
      showVal (Bool False) = "#f"
      showVal Nil          = "Nil"
      showVal (List vs)    = T.concat [ "(", T.unwords (map showVal vs), ")" ]
      showVal (Func _)     = "(internal function)"
      showVal (Lambda _ _) = "(lambda function)"
