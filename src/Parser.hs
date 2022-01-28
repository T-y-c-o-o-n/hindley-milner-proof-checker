module Parser where

import Base
import Control.Monad (replicateM_, void, when)
import Data.Void (Void)
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer (lexeme, symbol)

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) ProofTree
parse = Text.Megaparsec.parse (parseProof <* eof) ""

char :: Char -> Parser ()
char c = void $ symbol C.space [c]

string :: String -> Parser ()
string = void . symbol C.space

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
  return $ context :|- typedExpr :# rule

parseIndents :: Int -> Parser ()
parseIndents = flip replicateM_ $ C.string "*   "

parseContext :: Parser Context
parseContext =
  reverse
    <$> sepBy
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
    [ Mono <$> try parseMonoType,
      do
        char '('
        t <- parseType
        char ')'
        return t,
      do
        string "forall"
        x <- parseVariable
        char '.'
        ForAll x <$> parseType
    ]

parseMonoType :: Parser MonoType
parseMonoType =
  do
    left <-
      choice
        [ do
            char '('
            t <- parseMonoType
            char ')'
            return t,
          V <$> parseVariable
        ]
    foldl (:=>) left
      <$> optional
        ( do
            string "->"
            parseMonoType
        )

parseLambda :: Parser Expr
parseLambda =
  do
    char '\\'
    x <- parseVariable
    char '.'
    L x <$> parseExpr

parseExpr :: Parser Expr
parseExpr =
  do
    applMaybe <- optional parseApplication
    case applMaybe of
      Nothing ->
        choice
          [ try $ do
              string "let"
              x <- parseVariable
              char '='
              e0 <- parseExpr
              string "in"
              Let x e0 <$> parseExpr,
            parseLambda
          ]
      Just appl -> do
        foldl Appl appl <$> optional parseLambda

parseApplication :: Parser Expr
parseApplication =
  do
    atoms <- some parseAtom
    return $ foldl1 Appl atoms

parseAtom :: Parser Expr
parseAtom =
  ( do
      char '('
      expr <- parseExpr
      char ')'
      return expr
  )
    <|> (Var <$> try parseVariable)

parseVariable :: Parser Var
parseVariable =
  do
    c <- C.lowerChar
    s <- many (C.lowerChar <|> C.digitChar <|> C.char '\'')
    C.space
    let res = c : s
    if res == "let" || res == "in" || res == "forall" then fail "" else return res

parseRule :: Parser Int
parseRule = do
  string "[rule #"
  d <- choice $ map (C.string . show) [1 .. 6 :: Integer]
  char ']'
  return $ read d -- TODO replace with something
