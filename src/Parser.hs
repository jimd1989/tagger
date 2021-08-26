module Parser (FileParser(..), fileParser) where

-- Parses a string representing the tag format of the filenames to be tagged.
-- Example: "{d}-{n}. {a} - {t}.mp3"
-- Returns another parser, FileParser, to be run across all mp3 files.

import Prelude (Char, Eq, Int, Show, String, (.), (-), ($), (==), (>>=), show)
import Control.Applicative ((<|>), (*>), liftA2, pure)
import Control.Arrow ((+++), left)
import Data.Bitraversable (bisequence)
import Data.Either (Either)
import Data.Functor (($>))
import Data.List (filter, foldr1, length, intercalate, zipWith)
import Data.Maybe (Maybe(..))
import Data.Tuple (uncurry)
import Text.Parsec (ParsecT, Stream, many1, manyTill, parse, try)
import Text.Parsec.Char (anyChar, char, digit, oneOf, string)
import Text.Parsec.Combinator (between, choice, count, lookAhead, optionMaybe)
import EyeD3Tag (EyeD3Tag(..), Tagger(..), getTag)
import Helpers ((⊙), (◇))

newtype Delimeter = Delimeter { getDelimeter ∷ Maybe Char }
  deriving (Eq, Show)

newtype FileParser = FileParser { runParser ∷ String → Either String String }

-- Helpers
nextChar ∷ Stream s m Char ⇒ ParsecT s u m Char
nextChar = anyChar *> anyChar

untilChar ∷ Stream s m Char ⇒ Char → ParsecT s u m String
untilChar α = try $ manyTill anyChar (lookAhead $ char α)

includingChar ∷ Stream s m Char ⇒ Char → ParsecT s u m String
includingChar α = liftA2 snoc (untilChar α) anyChar
  where snoc α ω = α ◇ [ω]

untilEof ∷ Stream s m Char ⇒ ParsecT s u m String
untilEof = many1 anyChar

delimeter ∷ Stream s m Char ⇒ ParsecT s u m Delimeter
delimeter = Delimeter ⊙ optionMaybe (try $ lookAhead nextChar)

delimeterCount ∷ Stream s m Char ⇒ Delimeter → ParsecT s u m Int
delimeterCount α = case (getDelimeter α) of
  (Just  ω) → (length . (filter (== ω))) ⊙ lookAhead untilEof
  (Nothing) → pure 0

delimeterAndCount ∷ Stream s m Char ⇒ ParsecT s u m (Delimeter, Int)
delimeterAndCount = bisequence (delimeter, delimeter >>= delimeterCount)

tagger ∷ Stream s m Char ⇒ Char → (String → EyeD3Tag) → ParsecT s u m Tagger
tagger α ω = char α $> Tagger ω

-- Format string parsers
exactText ∷ Stream s m Char ⇒ ParsecT s u m (ParsecT s u m String)
exactText = plaintext ⊙ (untilChar '{' <|> untilEof)

textTag ∷ Stream s m Char ⇒ ParsecT s u m (ParsecT s u m String)
textTag = check *> liftA2 (uncurry . text) choices delimeterAndCount
  where chars        = "abAGt"
        constructors = [Artist, AlbumArtist, Album, Genre, Title]
        check        = lookAhead $ oneOf chars
        choices      = choice $ zipWith tagger chars constructors

numTag ∷ Stream s m Char ⇒ ParsecT s u m (ParsecT s u m String)
numTag = number ⊙ (check *> choices)
  where chars        = "dnY"
        constructors = [DiscNum, TrackNum, Year]
        check        = lookAhead $ oneOf chars
        choices      = choice $ zipWith tagger chars constructors

tag ∷ Stream s m Char ⇒ ParsecT s u m (ParsecT s u m String)
tag = between (char '{') (char '}') $ numTag <|> textTag

-- Generated file name parsers
plaintext ∷ Stream s m Char ⇒ String → ParsecT s u m String
plaintext α = string α $> ""

untilDelimeter ∷ Stream s m Char ⇒ Int → Delimeter → ParsecT s u m String
untilDelimeter n α = case (getDelimeter α) of
  (Just  ω) → liftA2 (◇) (cat ⊙ (count n $ includingChar ω)) (untilChar ω)
  (Nothing) → untilEof
  where cat = intercalate ""

text ∷ Stream s m Char ⇒ Tagger → Delimeter → Int → ParsecT s u m String
text f α n = getTag f ⊙ (delimeterCount α >>= nFields)
  where nFields ω = untilDelimeter (ω - n) α

number ∷ Stream s m Char ⇒ Tagger → ParsecT s u m String
number f = getTag f ⊙ (many1 digit)

-- The whole thing
formatString ∷ Stream s m Char ⇒ ParsecT s u m (ParsecT s u m String)
formatString = (foldr1 (◇)) ⊙ (many1 $ tag <|> exactText)

fileParser ∷ String → Either String FileParser
fileParser = (show +++ makeParser) . parse formatString outerMsg
  where outerMsg     = "Invalid format string"
        innerMsg     = "File doesn't match format string"
        makeParser α = FileParser $ (left show . parse α innerMsg)
