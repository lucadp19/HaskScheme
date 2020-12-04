{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Parser where

import LispVal
import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import qualified Data.Text as T
import Control.Applicative hiding ((<|>))
import Data.Functor.Identity (Identity)

{- 
    Parsing works like this:
        Parser LispVal = Parser (Text -> [(LispVal, Text)])

    So a parser takes some text and returns a LispVal and some other text
-}

{-
    Definitions:
        - a lexeme is the basic lexical unit of meaning
        - a token is a structure representing a lexeme that explicitly indicates its categorisation for the purpose of parsing
        - a lexer is an algorithm that separates a stream of text into lexemes. 
            It defines the rules for individual words or sets of allowed symbols in a prog. language.
        - a parser is an algorithm to convert lexemes into a valid language grammar.
            It defines the grammatical rules.
-}

        -- Tok.GetTokenParser s u m
        --      - s is the stream T.Text
        --      - u is the state ()
        --      - m is the monad Identity
lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style = Lang.emptyDef {
      Tok.commentStart      = "{-"
    , Tok.commentEnd        = "-}"
    , Tok.commentLine       = "--"
    , Tok.opStart           = Tok.opLetter style
    , Tok.opLetter          = oneOf ":!#$%%&*+./<=>?@\\^|-~"
    , Tok.identStart        = letter <|> oneOf "-+*/=|&><"
    , Tok.identLetter       = digit <|> letter <|> oneOf "?+=|&-/"
    , Tok.reservedOpNames   = ["'", "\""]
}

{-           PARSER          -}
-- still not getting how this thing works
m_parens :: Parser a -> Parser a
m_identifier :: Parser String
Tok.TokenParser { Tok.parens = m_parens
                , Tok.identifier = m_identifier} = Tok.makeTokenParser style

reservedOp :: T.Text -> Parser ()
reservedOp op = Tok.reservedOp lexer $ T.unpack op

parseAtom :: Parser LispVal
parseAtom = Atom . T.pack <$> m_identifier  
-- what is fmap here?
{-
    Okay, fmap is given from Parser (which is ParsecT Text ()) 
    and it means: idk        
-}

parseText :: Parser LispVal
parseText = do
    reservedOp "\""             -- first "
    p <- many1 $ noneOf "\""    -- many characters that aren't "
    reservedOp "\""             -- closing "
    return $ String $ T.pack p

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseNegNumber :: Parser LispVal
parseNegNumber = do
    char '-'
    d <- many1 digit
    return $ Number . negate . read $ d

parseList :: Parser LispVal
parseList = List . concat <$> Text.Parsec.many parseExpr `sepBy` (char ' ' <|> char '\n')
{-
    parsing a list means:
        - parsing many expressions separated by spaces or line breaks
        - using List . concat to get the list type back
-}

parseReserved :: Parser LispVal
parseReserved = 
        (reservedOp "Nil" >> return Nil)
    <|> (reservedOp "#t"  >> return (Bool True))
    <|> (reservedOp "#f"  >> return (Bool False))

parseQuote :: Parser LispVal
parseQuote = do
    reservedOp "\'"
    x <- parseExpr
    return $ List [Atom "quote", x]

parseSExpr :: Parser LispVal
parseSExpr = List . concat <$> m_parens ( Text.Parsec.many parseExpr `sepBy` (char ' ' <|> char '\n'))

parseExpr :: Parser LispVal
parseExpr = parseReserved
        <|> parseNumber
        <|> try parseNegNumber
        <|> parseAtom
        <|> parseText
        <|> parseQuote
        <|> parseSExpr


contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

readExpr :: T.Text -> Either ParseError LispVal
readExpr = parse (contents parseExpr) "<stdin>"

readExprFile :: T.Text -> Either ParseError LispVal
readExprFile = parse (contents parseExpr) "<file>"