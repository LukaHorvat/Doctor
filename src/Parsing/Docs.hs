module Parsing.Docs where

import Common.Prelude
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token (stringLiteral)
import Text.Parsec.Language (haskell)
import Control.Applicative hiding ((<|>), many)

data Doc = Doc [Chunk] deriving (Show, Eq, Ord, Read)
data Chunk = Untagged String | Tagged String Ref deriving (Show, Eq, Ord, Read)
data Ref = Ref { refFile  :: String
               , refLines :: [Marker] }  deriving (Show, Eq, Ord, Read)
data Marker = Marker String deriving (Show, Eq, Ord, Read)

fromString :: String -> Either ParseError Doc
fromString src = parse doc "" src

stringLit :: Parser String
stringLit = stringLiteral haskell

charOrEscape :: Parser Char
charOrEscape = char '\\' *> oneOf "\\}{" <|> noneOf "\\}{"

withSpaces :: Parser a -> Parser a
withSpaces p = p <* many (oneOf " \t")

word :: String -> Parser String
word = withSpaces . string

number :: Parser Int
number = withSpaces $ read <$> many1 digit

rawRef :: Parser Ref
rawRef = between (word "(-") (string "-)") ref
    where ref     = Ref <$> path <*> markers
          markers = many marker
          marker  = try $ Marker <$> many1 markerChar <* word ";"
          markerChar = noneOf "-;" <|> try (char '-' <* notFollowedBy (char ')'))
          path    = withSpaces $ lookAhead (char '"') *> stringLit <|> many1 (noneOf " ")

tagged :: Parser Chunk
tagged = Tagged <$> closed <*> rawRef
    where closed = between (char '{') (char '}') $ many1 charOrEscape

untagged :: Parser Chunk
untagged = Untagged <$> many1 charOrEscape

chunk :: Parser Chunk
chunk = tagged <|> untagged

doc :: Parser Doc
doc = Doc <$> many chunk
