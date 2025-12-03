{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative (Alternative (..))
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)

type Error = String

newtype Parser a = Parser {parse :: String -> Either Error (a, String)}

-- Instances

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f px = Parser $
    \input -> do
      (x, rest) <- parse px input
      Right (f x, rest)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $
    \input -> Right (x, input)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = Parser $
    \input -> do
      (f, rest) <- parse pf input
      (x, rest') <- parse px rest
      Right (f x, rest')

instance Monad Parser where
  return :: a -> Parser a
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  px >>= f = Parser $
    \input -> do
      (x, rest) <- parse px input
      parse (f x) rest

instance MonadFail Parser where
  fail :: String -> Parser a
  fail msg = Parser $ \_ -> Left msg

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Left ""

  (<|>) :: Parser a -> Parser a -> Parser a
  px <|> py = Parser $
    \input -> case parse px input of
      Right t -> Right t
      Left msg -> case parse py input of
        Right t' -> Right t'
        Left msg' -> Left $ msg ++ " or " ++ msg'

--- Parser functions

item :: Parser Char
item = Parser $ \case
  (x : xs) -> Right (x, xs)
  [] -> Left "Unexpected end of input"

char :: Char -> Parser Char
char c = do
  x <- item
  if x == c
    then return x
    else fail $ "Expected: " ++ [c] ++ " Received: " ++ [x]

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  x <- item
  if f x
    then return x
    else fail $ "Input: '" ++ [x] ++ "' does not satisfy predicate"

digit :: Parser Char
digit = satisfy isDigit

digits0 :: Parser [Char]
digits0 = many digit

digits1 :: Parser [Char]
digits1 = some digit

space :: Parser ()
space = void $ satisfy (== ' ')

spaces0 :: Parser ()
spaces0 = void $ many space

spaces1 :: Parser ()
spaces1 = void $ some space

newline :: Parser ()
newline = void $ satisfy (== '\n') <|> satisfy (== '\r')

newline0 :: Parser ()
newline0 = void $ many newline

newline1 :: Parser ()
newline1 = void $ some newline

alpha :: Parser Char
alpha = satisfy isAlpha

alpha0 :: Parser String
alpha0 = many alpha

alpha1 :: Parser String
alpha1 = some alpha

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

alphaNum0 :: Parser String
alphaNum0 = many alphaNum

alphaNum1 :: Parser String
alphaNum1 = some alphaNum

string :: String -> Parser String
string = mapM char

nat :: Parser Int
nat = read <$> digits1

int :: Parser Int
int = nat <|> char '-' *> (negate <$> nat)

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 px psep =
  (:)
    <$> px
    <*> many (psep *> px)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy px psep = sepBy1 px psep <|> pure []

lines1 :: Parser a -> Parser [a]
lines1 px = sepBy1 px newline

try :: Parser a -> Parser a
try p = Parser $ \input ->
  case parse p input of
    Right result -> Right result
    Left _ -> Left ""

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

splitOn :: Char -> Parser a -> Parser (a, a)
splitOn c px = do
  xs <- sepBy px (char c)
  case xs of
    [a, b] -> return (a, b)
    _ -> fail $ "Expected exactly two elements seperated by the delimiter:" ++ [c]

digitsAsList :: Parser [Int]
digitsAsList =
  map (\c -> fromEnum c - fromEnum '0')
    <$> digits1

-- Utilities

unwrapParser :: Parser a -> String -> a
unwrapParser p input =
  case parse p input of
    Right (x, _) -> x
    Left err -> error $ "Parse failed: " ++ err
