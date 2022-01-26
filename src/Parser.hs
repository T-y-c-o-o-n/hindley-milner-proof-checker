module Parser where

import Base
import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) ProofTree
parse = Text.Megaparsec.parse (parseProof >>= \res -> eof >> return res) ""

ws :: Parser ()
ws =
  L.space
    Text.Megaparsec.Char.space1
    (L.skipLineComment ";;")
    (L.skipBlockCommentNested "/*" "*/")

parseProof :: Parser ProofTree
parseProof = do
  node <- parseString
  return undefined

parseString :: Parser ProofNode
parseString = do
  parseIndent
  parseContext
  parseTypedExpr
  parseRule
  return undefined

parseIndent :: Parser ()
parseIndent = void $ string "*   "

parseContext :: Parser ()
parseContext = -- optional $
  do
    parseVariable
    _ <- char ':'
    parseType
    -- optional $ do char '.'; parseType

parseTypedExpr :: Parser ()
parseTypedExpr = do
  parseExpr
  _ <- char ':'
  parseType

parseType :: Parser ()
parseType =
  choice
    [ do
        _ <- char '('
        parseType
        _ <- char ')',
      parseMonoType,
      do
        _ <- string "forall"
        parseVariable
        _ <- char '.'
        parseType
    ]

parseMonoType :: Parser ()
parseMonoType =
  choice
    [ parseVariable,
      do
        _ <- char '('
        parseMonoType
        _ <- char ')'
        _ <- string "->"
        parseMonoType,
      do
        _ <- char '('
        t <- parseMonoType
        _ <- char ')'
        return t,
      do
        parseVariable
        _ <- string "->"
        parseMonoType
    ]

parseExpr :: Parser ()
parseExpr =
  choice
    [ do
        --        optional parseApplication  TODO
        _ <- char '\\'
        parseVariable
        _ <- char '.'
        parseExpr,
      do
        _ <- string "let"
        parseVariable
        _ <- char '='
        parseExpr
        _ <- string "in"
        parseExpr
    ]

parseApplication :: Parser ()
parseApplication =
  parseAtom
    <|> ( do
            parseApplication
            parseAtom
        )

parseAtom :: Parser ()
parseAtom =
  ( do
      _ <- char '('
      expr <- parseExpr
      _ <- char ')'
      return expr
  )
    <|> parseVariable

parseVariable :: Parser ()
parseVariable = void $ string "*   "

parseRule :: Parser Int
parseRule = do
  _ <- string "[rule #"
  d <- choice $ map (string . show) [1 .. 6]
  _ <- char ']'
  return $ read d -- TODO replace with something
