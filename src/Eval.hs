{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Eval where

import LispVal
import Data.Text as T
import Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Except ()

-- | The evaluation function.
-- It takes a @LispVal@ value and returns an @Eval LispVal@,
-- which is the result of the evaluation.
eval :: LispVal -> Eval LispVal
    {- Autoquote facility of Scheme/Lisp -}
eval (Number i) = return $ Number i
eval (String s) = return $ String s
eval (Bool b)   = return $ Bool b
eval (List [])  = return Nil
eval Nil        = return Nil
    {- Quote -}
eval (List [Atom "quote", val]) = return val
    {- Write -}
eval (List [Atom "write", rest])    = return . String . T.pack . show $ rest
eval (List ((Atom "write") : rest)) = return . String . T.pack . show $ List rest
    {- Variable lookup -}
eval n@(Atom _) = lookupVar n
    {- If expression -}
eval (List [Atom "if", cond, trueExpr, falseExpr]) =
    eval cond >>= \case
        Bool True -> eval trueExpr
        Bool False -> eval falseExpr
        _           -> throw $ BadSpecialForm "if"
    {- Let expression -}
eval (List [Atom "let", List pairs, expr]) = do
    env <- ask
    atoms <- mapM ensureAtom $ getEven pairs        -- gets the variable names
    vals <- mapM ensureAtom $ getOdd pairs          -- gets the values
    let env' = Map.fromList (zipVarValues atoms vals) <> env
        in local (const env') $ evalBody expr
    {- Begin expression -}
eval (List [Atom "begin", rest]) = evalBody rest
eval (List ((Atom "begin") : rest)) = evalBody $ List rest
    {- Define expression-}
eval (List [Atom "define", varExpr, expr]) = do
    varAtom <- ensureAtom varExpr
    evalVal <- eval expr 
    env     <- ask
    let envFn = const $ Map.insert (extractVarName varAtom) evalVal env
        in local envFn $ return varExpr
eval (List [Atom "lambda", List params, expr]) = do
    envLocal <- ask     -- the local environment of the function
    return $ Lambda (IFunc $ applyLambda expr params) envLocal
    {- ACHTUNG:
        @applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal@
        So @applyLambda expr params :: [LispVal] -> Eval LispVal@ :
            this means we have a function with body @expr@ and parameter names @params@
            so we just need to supply the values for the given parameters to actually apply the function.
        Furthermore, the @Lambda@ constructor contains also an @EnvCtx@ parameter, which is the
        lexical scope used to form a closure. 
    -}
eval (List [Atom "lambda", _]) = throw $ BadSpecialForm "lambda"
    {- Function application -}
eval (List (x : xs)) = do
    funVar <- eval x
    xVal   <- mapM eval xs
    case funVar of
        Fun (IFunc internalFn) -> internalFn xVal
        Lambda (IFunc internalFn) lamEnv -> local (const lamEnv) $ internalFn xVal
        _ -> throw $ NotAFunction funVar


-- | @applyLambda@ takes the body of a function, the list of parameters,
-- the list of actual arguments and returns the result of the function evaluation.
applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
applyLambda expr params args = do
    env <- ask
    argEval <- mapM eval args
    let env' = Map.fromList (zipVarValues params argEval) <> env
        in local (const env') $ eval expr

-- still haven't quite undestood what the hell is this for
evalBody :: LispVal -> Eval LispVal
    {- A define expression followed by a single other expression -}
evalBody (List [List ((Atom "define") : [Atom var, defExpr]), rest]) = do
    evalVal <- eval defExpr
    env     <- ask
    let envFn = const $ Map.insert var evalVal env -- inserts the mapping (var, evalVal) in the environment
        in local envFn $ eval rest 
    {- A define expression followed by multiple other expressions -}
evalBody (List (List ((Atom "define") : [Atom var, defExpr]) : rest)) = do
    evalVal <- eval defExpr
    env     <- ask
    let envFn = const $ Map.insert var evalVal env
        in local envFn $ evalBody $ List rest

-- Helper function for looking up an atom in the environment
lookupVar :: LispVal -> Eval LispVal
lookupVar (Atom var) = do
    env <- ask
    case Map.lookup var env of
        Just x -> return x
        Nothing -> throw $ UnboundVar atom

-- Helper function for ensuring a @LispVal@ is an @Atom@
ensureAtom :: LispVal -> Eval LispVal
ensureAtom n@(Atom _) = return n
ensureAtom _          = throw $ TypeMismatch "atom" n

-- Helper functions for getting the even/odd elements out of a list
getEven :: [a] -> [a]
getEven [] = []
getEven (x:xs) = x : getOdd xs

getOdd :: [a] -> [a]
getOdd [] = []
getOdd (_:xs) = getEven xs

-- Helper function for getting the name of a variable
extractVarName :: LispVal -> T.Text
extractVarName (Atom name) = name

-- Helper function used to zip together variable names and values
zipVarValues :: [LispVal] -> [LispVal] -> [(T.Text, LispVal)]
zipVarValues = Prelude.zipWith (\var value -> (extractVarName var, value))