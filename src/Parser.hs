module Parser where

-- TODO: CONFIRM BEHAVIOR, CLEAN UP REDUNDANCIES

-- Parses a string representing the tag format of the filenames to be tagged.
-- Example: "{d}-{n}. {a} - {t}.mp3"
-- Returns another parser, FileParser, to be run across all mp3 files.

import Prelude (Char, Eq, Int, String, (.), (-), ($), (==), (>>=), pred, show)
import Control.Applicative ((<|>), (*>), liftA2, pure)
import Control.Arrow ((+++), left)
import Data.Bitraversable (bisequence)
import Data.Either (Either)
import Data.Functor (($>))
import Data.List (filter, foldr1, length, intercalate, zipWith)
import Data.List.Split (splitOn)
import Data.Maybe (Maybe(..))
import Data.Tuple (uncurry)
import Text.Parsec (ParsecT, Stream, many1, manyTill, parse, try)
import Text.Parsec.Char (anyChar, char, digit, oneOf, string)
import Text.Parsec.Combinator (between, choice, count, lookAhead, optionMaybe)
import EyeD3Tag (EyeD3Tag(..), Tagger(..), getTag)
import Helpers ((⊙), (◇), fork)

-- Needs to be more than a char?
-- A parser itself?
-- A string?
newtype Delimiter = Delimiter { getDelimiter ∷ Maybe Char }

newtype Delimiter' = Delimiter' { getDelimiter' ∷ Maybe String }

newtype FileParser = FileParser { runParser ∷ String → Either String String }

-- Helpers
skipCharAndThen ∷ Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m a
skipCharAndThen f = anyChar *> f

-- try was flipped here. Used to be at front. Remember this?
until ∷ Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m String
until f = try $ manyTill anyChar (try $ lookAhead $ f)

untilChar ∷ Stream s m Char ⇒ Char → ParsecT s u m String
untilChar α = until (char α)

untilString ∷ Stream s m Char ⇒ String → ParsecT s u m String
untilString α = until (string α)

untilEof ∷ Stream s m Char ⇒ ParsecT s u m String
untilEof = many1 anyChar

-- Is this used in all the proper places?
untilTag ∷ Stream s m Char ⇒ ParsecT s u m String
untilTag = untilChar '{' <|> untilEof 

includingChar ∷ Stream s m Char ⇒ Char → ParsecT s u m String
includingChar α = liftA2 snoc (untilChar α) anyChar
  where snoc α ω = α ◇ [ω]

-- Is this correct? May be a source of errors. Come back to later.
includingString ∷ Stream s m Char ⇒ String → ParsecT s u m String
includingString = fork (liftA2 (◇)) untilString string

delimiter ∷ Stream s m Char ⇒ ParsecT s u m Delimiter
delimiter = Delimiter ⊙ optionMaybe (try $ lookAhead (skipCharAndThen anyChar))

delimiter' ∷ Stream s m Char ⇒ ParsecT s u m Delimiter'
delimiter' = Delimiter' ⊙ optionMaybe (try $ lookAhead (skipCharAndThen untilTag))

delimiterCount ∷ Stream s m Char ⇒ Delimiter → ParsecT s u m Int
delimiterCount α = case (getDelimiter α) of
  (Just  ω) → (length . (filter (== ω))) ⊙ lookAhead untilEof
  (Nothing) → pure 0

delimiterCount' ∷ Stream s m Char ⇒ Delimiter' → ParsecT s u m Int
delimiterCount' α = case (getDelimiter' α) of
  (Just  ω) → (pred . length . splitOn ω) ⊙ lookAhead untilEof
  (Nothing) → pure 0

delimiterAndCount ∷ Stream s m Char ⇒ ParsecT s u m (Delimiter, Int)
delimiterAndCount = bisequence (delimiter, delimiter >>= delimiterCount)

delimiterAndCount' ∷ Stream s m Char ⇒ ParsecT s u m (Delimiter', Int)
delimiterAndCount' = bisequence (delimiter', delimiter' >>= delimiterCount')

tagger ∷ Stream s m Char ⇒ Char → (String → EyeD3Tag) → ParsecT s u m Tagger
tagger α ω = char α $> Tagger ω

-- Format string parsers
exactText ∷ Stream s m Char ⇒ ParsecT s u m (ParsecT s u m String)
exactText = plaintext ⊙ untilTag

textTag ∷ Stream s m Char ⇒ ParsecT s u m (ParsecT s u m String)
textTag = check *> liftA2 (uncurry . text) choices delimiterAndCount'
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

untilDelimiter ∷ Stream s m Char ⇒ Int → Delimiter → ParsecT s u m String
untilDelimiter n α = case (getDelimiter α) of
  (Just  ω) → liftA2 (◇) (cat ⊙ (count n $ includingChar ω)) (untilChar ω)
  (Nothing) → untilEof
  where cat = intercalate ""

untilDelimiter' ∷ Stream s m Char ⇒ Int → Delimiter' → ParsecT s u m String
untilDelimiter' n α = case (getDelimiter' α) of
  (Just  ω) → liftA2 (◇) (cat ⊙ (count n $ includingString ω)) (untilString ω)
  (Nothing) → untilEof
  where cat = intercalate ""

text ∷ Stream s m Char ⇒ Tagger → Delimiter' → Int → ParsecT s u m String
text f α n = getTag f ⊙ (delimiterCount' α >>= nFields)
  where nFields ω = untilDelimiter' (ω - n) α

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
