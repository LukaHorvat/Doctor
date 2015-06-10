module Parsing.Docs where

import Common.Prelude
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.String
import Text.Parsec.Token (stringLiteral)
import Text.Parsec.Language (haskell)

data Doc = Doc [Chunk] deriving (Show, Eq, Ord, Read)
data Command = ChangeProvider String | ChangeFile String | Break deriving (Show, Eq, Ord, Read)
data Chunk = Untagged String | Tagged String Ref | Command Command deriving (Show, Eq, Ord, Read)
data Ref = Ref { refInternal :: Bool
               , refProvider :: String
               , refFile     :: String
               , refMarkers  :: [String]}  deriving (Show, Eq, Ord, Read)

refToString :: Ref -> String
refToString Ref { refProvider = prov, refFile = file, refMarkers = markers } =
    "!" ++ show prov ++ " " ++ show file ++ " " ++ concatMap (++ ";") markers

fromString :: String -> Either ParseError Doc
fromString = parse doc ""

stringLit :: Parser String
stringLit = stringLiteral haskell

path :: Parser String
path = withSpaces $ char '!' *> (lookAhead (char '"') *> stringLit <|> many (noneOf " "))

charOrEscape :: Parser Char
charOrEscape = char '\\' *> oneOf "\\}{("
           <|> noneOf "\\}{("
           <|> try (char '(' <* notFollowedBy (char '-'))

withSpaces :: Parser a -> Parser a
withSpaces p = p <* many (oneOf " \t")

word :: String -> Parser String
word = withSpaces . string

number :: Parser Int
number = withSpaces $ read <$> many1 digit

rawRef :: Parser Ref
rawRef = between (word "(-") (string "-)") ref
    where internal   = word "~" *> return True <|> return False
          ref        = Ref <$> internal <*> provider <*> path <*> markers
          markers    = many marker
          marker     = try $ many1 markerChar <* word ";"
          markerChar = noneOf "-;" <|> try (char '-' <* notFollowedBy (char ')'))
          provider   = path

tagged :: Parser Chunk
tagged = Tagged <$> closed <*> rawRef
    where closed = between (char '{') (char '}') $ many1 charOrEscape

commandChar :: Parser Char
commandChar = noneOf "-" <|> try (char '-' <* notFollowedBy (char ')'))

command :: Parser Command
command = ChangeProvider <$> (word "provider" *> many1 commandChar)
      <|> ChangeFile     <$> (word "file" *> many1 commandChar)
      <|> word "break" *> return Break

commandChunk :: Parser Chunk
commandChunk = Command <$> between (word "(-") (string "-)") command

untagged :: Parser Chunk
untagged = Untagged <$> many1 charOrEscape

chunk :: Parser Chunk
chunk = tagged <|> commandChunk <|> untagged

doc :: Parser Doc
doc = Doc <$> many chunk
