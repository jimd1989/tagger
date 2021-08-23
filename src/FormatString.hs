module FormatString where

-- Takes a string representing the tag format of the filenames to be tagged.
-- Example: "{d}-{n}. {a} - {t}.mp3"

import Prelude (Char, Int, Show, String, (.), (+), (-), ($), (>>=), show)
import Control.Applicative ((<|>), (*>), liftA2, pure)
import Data.Bitraversable (bisequence)
import Data.Eq (Eq, (==))
import Data.Functor (($>))
import Data.List (filter, foldr1, length, intercalate, zipWith)
import Data.Maybe (Maybe(..))
import Data.Tuple (uncurry)
import Text.Parsec (ParsecT, Stream, eof, many1, manyTill, try)
import Text.Parsec.Char (anyChar, char, digit, oneOf, string)
import Text.Parsec.Combinator (between, choice, count, lookAhead, optionMaybe)
import EyeD3Tag (EyeD3Tag(..), Tagger(..))
import Helpers ((⊙), (◇))

newtype Delimeter = Delimeter { getDelimeter ∷ Maybe Char }
  deriving (Eq, Show)

-- Helpers
tagger ∷ Stream s m Char ⇒ Char → (String → EyeD3Tag) → ParsecT s u m Tagger
tagger α ω = char α $> Tagger ω

nextChar ∷ Stream s m Char ⇒ ParsecT s u m Char
nextChar = anyChar *> anyChar

delimeter ∷ Stream s m Char ⇒ ParsecT s u m Delimeter
delimeter = Delimeter ⊙ optionMaybe (try $ lookAhead nextChar)

delimeterCount ∷ Stream s m Char ⇒ Delimeter → ParsecT s u m Int
delimeterCount α = case (getDelimeter α) of
  (Just  ω) → (length . (filter (== ω))) ⊙ lookAhead (manyTill anyChar eof)
  (Nothing) → pure 0

delimeterAndCount ∷ Stream s m Char ⇒ ParsecT s u m (Delimeter, Int)
delimeterAndCount = bisequence (delimeter, delimeter >>= delimeterCount)

untilChar ∷ Stream s m Char ⇒ Char → ParsecT s u m String
untilChar α = try $ manyTill anyChar (lookAhead $ char α)

untilEof ∷ Stream s m Char ⇒ ParsecT s u m String
untilEof = many1 anyChar

-- Generators
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

-- Generated
untilDelimeter ∷ Stream s m Char ⇒ Delimeter → ParsecT s u m String
untilDelimeter α = case (getDelimeter α) of
  (Just  ω) → untilChar ω
  (Nothing) → untilEof

text ∷ Stream s m Char ⇒ Tagger → Delimeter → Int → ParsecT s u m String
text f α n = eyeD3Tag ⊙ (delimeterCount α >>= nFields)
  where eyeD3Tag  = show . runTagger f . intercalate ""
        nFields ω = count (1 + (ω - n)) $ untilDelimeter α

number ∷ Stream s m Char ⇒ Tagger → ParsecT s u m String
number f = eyeD3Tag ⊙ (many1 digit)
  where eyeD3Tag = show . runTagger f

plaintext ∷ Stream s m Char ⇒ String → ParsecT s u m String
plaintext α = string α $> ""

-- The whole thing
-- Compiles, but not quite accurate with delimeters
formatString ∷ Stream s m Char ⇒ ParsecT s u m (ParsecT s u m String)
formatString = (foldr1 (◇)) ⊙ (many1 $ tag <|> exactText)
