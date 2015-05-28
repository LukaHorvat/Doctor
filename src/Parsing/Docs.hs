module Parsing.Docs where

import Common.Prelude
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.String
import Text.Parsec.Token (stringLiteral)
import Text.Parsec.Language (haskell)

data Doc = Doc [Chunk] deriving (Show, Eq, Ord, Read)
data Chunk = Untagged String | Tagged String Ref deriving (Show, Eq, Ord, Read)
data Ref = Ref { refProvider :: String
               , refFile     :: String
               , refMarkers  :: [String] }  deriving (Show, Eq, Ord, Read)

refToString :: Ref -> [Char]
refToString Ref { refProvider = prov, refFile = file, refMarkers = markers } =
    "!" ++ show prov ++ " " ++ show file ++ " " ++ concatMap (++ ";") markers

fromString :: String -> Either ParseError Doc
fromString src = parse doc "" src

stringLit :: Parser String
stringLit = stringLiteral haskell

path :: Parser String
path = withSpaces $ lookAhead (char '"') *> stringLit <|> many1 (noneOf " ")

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
    where ref        = Ref <$> provider <*> path <*> markers
          markers    = many marker
          marker     = try $ many1 markerChar <* word ";"
          markerChar = noneOf "-;" <|> try (char '-' <* notFollowedBy (char ')'))
          provider   = char '!' *> path <|> return ""

tagged :: Parser Chunk
tagged = Tagged <$> closed <*> rawRef
    where closed = between (char '{') (char '}') $ many1 charOrEscape

untagged :: Parser Chunk
untagged = Untagged <$> many1 charOrEscape

chunk :: Parser Chunk
chunk = tagged <|> untagged

doc :: Parser Doc
doc = Doc <$> many chunk
