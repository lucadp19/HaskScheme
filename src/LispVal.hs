{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module LispVal where

import Data.Typeable (Typeable)
import qualified Data.Text as T
import qualified Data.Map as Map

import Control.Monad.Reader
import Control.Monad.Except

-- | A @LispVal@ is a basic S-Expression datatype.
data LispVal 
    = Atom T.Text           -- A variable
    | List [LispVal]        -- An S-Expr
    | Number Integer        -- type wrapper for integers
    | String T.Text         -- type wrapper for strings
    | Fun IFunc             -- primitive functions, such as +
    | Lambda IFunc EnvCtx   -- lambda functions with their environment
    | Nil                   -- void type
    | Bool Bool             -- type wrapper for booleans
    deriving (Typeable)

-- | Datatype for a function
newtype IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }

-- | Environment type
type EnvCtx = Map.Map T.Text LispVal 

-- | The evaluation monad
newtype Eval a = Eval { unEval :: ReaderT EnvCtx IO a }
    deriving ( Monad
             , Functor
             , Applicative
             , MonadReader EnvCtx
             , MonadIO )

{-
    Eval a is a monad composed by different monads:
        - ReaderT EnvCtx for the lexical scope (getting the values from the environment)
            - ReaderT = (e -> m a), dove in questo caso 
                e   = EnvCtx
                m a = IO LispVal
            - useful functions with ReaderT 
                ask   := returns the environment
                local := sets the environment and evaluates an expression
-}

{-                  SHOW INSTANCE                 -}
instance Show LispVal where
    show = T.unpack . showVal

showVal :: LispVal -> T.Text
showVal = \case
    Atom atom       -> atom
    String string   -> T.concat [ "\"", string, "\""]
    Number num      -> T.pack $ show num
    Bool True       -> "#T"
    Bool False      -> "#F"
    Nil             -> "Nil"
    List contents   -> T.concat [ "[", showContents contents, "]"]
    Fun _           -> "<Primitive function>"
    Lambda _ _         -> "<Lambda>"

showContents :: [LispVal] -> T.Text
showContents contents = T.intercalate ", " $ showVal <$> contents
{-
    Note on showContents contents:
        T.intercalate ", " $ showVal <$> contents
    means:
        - showVal <$> contents:
            applies showVal on every element of contents (fmap), making them T.Texts
        - T.intercalate ", " $ (showVal <$> contents):
            takes the list of T.Texts and combines them together in a single Text,
            where the separator is given by ", "

    WYAS uses T.unwords, which is the same as T.intercalate " "
         
-}