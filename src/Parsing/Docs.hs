module Parsing.Docs where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token (stringLiteral)
import Text.Parsec.Language (haskell)
import Control.Applicative hiding ((<|>), many)

data Doc = Doc [Chunk]
data Chunk = Untagged String | Tagged String Ref
data Ref = Ref { refFile   :: String
               , refLines  :: [Int] }
data CommitHash = CommitHash String

parse :: String -> Doc
parse path = undefined

stringLit :: Parser String
stringLit = stringLiteral haskell

charOrEscape :: Parser Char
charOrEscape = char '\\' *> oneOf "\\}" <|> anyChar

withSpaces :: Parser a -> Parser a
withSpaces p = p <* many (oneOf " \t")

word :: String -> Parser String
word = withSpaces . string

number :: Parser Int
number = withSpaces $ read <$> many1 digit

rawRef :: Parser Ref
rawRef = between (word "(-") (word "-)") ref'
    where ref' = Ref <$> stringLit <*> lines'
          lines' = concat <$> lineRange `sepBy` word ","
          lineRange = try ((\x y -> [x..y]) <$> (number <* word "-") <*> number)
                  <|> return <$> number

{-
closedRef :: Parser Ref
closedRef =
    where closed = between (char '{') (char '}') charOrEscape


ref :: Parser Chunk
ref = closedRef <|> lineRef
-}
{-
plainText :: Parser String
plainText = do
    i <- many $ noneOf "{"
-}
