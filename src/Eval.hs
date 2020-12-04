{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Eval where

import LispVal

eval :: LispVal -> Eval LispVal
eval = \case
    {- Autoquote facility of Scheme/Lisp -}
    Number i -> return $ Number i
    String s -> return $ String s
    Bool b   -> return $ Bool b
    List []  -> return Nil
    Nil      -> return Nil
    {- Quote -}
    List [Atom "quote", val] -> return val