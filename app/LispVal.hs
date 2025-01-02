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
      showVal (Func _)      = "(internal function)"
      showVal (Lambda _ _) = "(lambda function)"
