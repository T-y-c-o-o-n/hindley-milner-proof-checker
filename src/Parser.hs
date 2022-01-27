module Parser where

import Base
import Control.Monad (replicateM_, void)
import Data.Void (Void)
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) ProofTree
parse = Text.Megaparsec.parse (parseProof <* eof) ""

char :: Char -> Parser ()
char = (C.space *>) . void . C.char

string :: String -> Parser ()
string = (C.space *>) . void . C.string

parseProof :: Parser ProofTree
parseProof = parseProof' 0

parseProof' :: Int -> Parser ProofTree
parseProof' indent = do
  node <- parseLine indent
  children <- many $ try $ parseProof' $ indent + 1
  return $ children `Proof` node

parseLine :: Int -> Parser ProofNode
parseLine indents = do
  parseIndents indents
  context <- parseContext
  string "|-"
  typedExpr <- parseTypedExpr
  rule <- parseRule
  _ <- C.newline
  return $ context :|- typedExpr :# rule

parseIndents :: Int -> Parser ()
parseIndents = flip replicateM_ $ C.string "*   "

parseContext :: Parser Context
parseContext =
  sepBy
    ( do
        x <- parseVariable
        char ':'
        t <- parseType
        return $ Var x :. t
    )
    (char ',')

parseTypedExpr :: Parser TypedExpr
parseTypedExpr = do
  e <- parseExpr
  char ':'
  t <- parseType
  return $ e :. t

parseType :: Parser Type
parseType =
  choice
    [ try $ do
        char '('
        t <- parseType
        char ')'
        return t,
      try $ do
        string "forall"
        x <- parseVariable
        char '.'
        ForAll x <$> parseType,
      Mono <$> parseMonoType
    ]

parseMonoType :: Parser MonoType
parseMonoType =
  choice
    [ try $ do
        char '('
        t0 <- parseMonoType
        char ')'
        string "->"
        t1 <- parseMonoType
        return $ t0 :=> t1,
      try $ do
        char '('
        t <- parseMonoType
        char ')'
        return t,
      try $ do
        x <- parseVariable
        string "->"
        (V x :=>) <$> parseMonoType,
      V <$> parseVariable
    ]

parseExpr :: Parser Expr
parseExpr =
  choice
    [ try $ do
        maybeExpr <- optional parseApplication
        char '\\'
        x <- parseVariable
        char '.'
        e <- parseExpr
        return $ foldr Appl (L x e) maybeExpr,
      try $ do
        string "let"
        x <- parseVariable
        char '='
        e0 <- parseExpr
        string "in"
        Let x e0 <$> parseExpr,
      parseApplication
    ]

parseApplication :: Parser Expr
parseApplication =
  do
    atoms <- some parseAtom
    return $ foldl1 Appl atoms

parseAtom :: Parser Expr
parseAtom =
  try
    ( do
        char '('
        expr <- parseExpr
        char ')'
        return expr
    )
    <|> (Var <$> parseVariable)

parseVariable :: Parser Var
parseVariable =
  do
    C.space
    c <- C.lowerChar
    s <- manyTill (C.lowerChar <|> C.digitChar <|> C.char '\'') C.space
    return $ c : s

parseRule :: Parser Int
parseRule = do
  string "[rule #"
  d <- choice $ map (C.string . show) [1 .. 6 :: Integer]
  char ']'
  return $ read d -- TODO replace with something
