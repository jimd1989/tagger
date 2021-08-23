module FormatString where

-- Takes a string representing the tag format of the filenames to be tagged.
-- Example: "{d}-{n}. {a} - {t}.mp3"
-- Returns a list of Matchers to handle the tag parsing of each filename.

import Prelude (Char, Eq, Int, String, (.), (-), ($), (*>), (==), (>>=))
import Control.Applicative ((<|>), liftA2, pure)
import Control.Monad (join)
import Data.Bitraversable (bisequence)
import Data.Functor (($>))
import Data.List (filter, foldr1, length, intercalate, zipWith)
import Data.Maybe (Maybe(..))
import Data.Tuple (uncurry)
import Text.Parsec (ParsecT, Stream, eof, many1, manyTill, try)
import Text.Parsec.Char (anyChar, char, digit, oneOf, string)
import Text.Parsec.Combinator (between, choice, count, lookAhead, optionMaybe)
import EyeD3Tag (EyeD3Tag(..), Tagger(..))
import Helpers ((⊙), (◇))
import Matcher (Delimeter(..), Matcher(..))

tagger ∷ Stream s m Char ⇒ Char → (String → EyeD3Tag) → ParsecT s u m Tagger
tagger α ω = char α $> Tagger ω

-- Helpers
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

-- Old generators
exactText ∷ Stream s m Char ⇒ ParsecT s u m Matcher
exactText = ExactText ⊙ (untilChar '{' <|> untilEof)

textTag ∷ Stream s m Char ⇒ ParsecT s u m Matcher
textTag = liftA2 (uncurry . Until) (check *> choices) delimeterAndCount
  where chars        = "abAGt"
        constructors = [Artist, AlbumArtist, Album, Genre, Title]
        check        = lookAhead $ oneOf chars
        choices      = choice $ zipWith tagger chars constructors

numTag ∷ Stream s m Char ⇒ ParsecT s u m Matcher
numTag = liftA2 Number (check *> choices) delimeter
  where chars        = "dnY"
        constructors = [DiscNum, TrackNum, Year]
        check        = lookAhead $ oneOf chars
        choices      = choice $ zipWith tagger chars constructors

tag ∷ Stream s m Char ⇒ ParsecT s u m Matcher
tag = between (char '{') (char '}') $ numTag <|> textTag

-- New generators
exactText' ∷ Stream s m Char ⇒ ParsecT s u m (ParsecT s u m EyeD3Tag)
exactText' = plaintext ⊙ (untilChar '{' <|> untilEof)

textTag' ∷ Stream s m Char ⇒ ParsecT s u m (ParsecT s u m EyeD3Tag)
textTag' = check *> liftA2 (uncurry . text) choices delimeterAndCount
  where chars        = "abAGt"
        constructors = [DiscNum, TrackNum, Year]
        check        = lookAhead $ oneOf chars
        choices      = choice $ zipWith tagger chars constructors

numTag' ∷ Stream s m Char ⇒ ParsecT s u m (ParsecT s u m EyeD3Tag)
numTag' = number ⊙ (check *> choices)
  where chars        = "dnY"
        constructors = [DiscNum, TrackNum, Year]
        check        = lookAhead $ oneOf chars
        choices      = choice $ zipWith tagger chars constructors

tag' ∷ Stream s m Char ⇒ ParsecT s u m (ParsecT s u m EyeD3Tag)
tag' = between (char '{') (char '}') $ numTag' <|> textTag'

-- Generated
untilDelimeter ∷ Stream s m Char ⇒ Delimeter → ParsecT s u m String
untilDelimeter α = case (getDelimeter α) of
  (Just  ω) → untilChar ω
  (Nothing) → untilEof

text ∷ Stream s m Char ⇒ Tagger → Delimeter → Int → ParsecT s u m EyeD3Tag
text f α n = eyeD3Tag ⊙ (delimeterCount α >>= nFields)
  where eyeD3Tag  = runTagger f . intercalate ""
        nFields ω = count (ω - n) $ untilDelimeter α

number ∷ Stream s m Char ⇒ Tagger → ParsecT s u m EyeD3Tag
number f = eyeD3Tag ⊙ (many1 digit)
  where eyeD3Tag = runTagger f

plaintext ∷ Stream s m Char ⇒ String → ParsecT s u m EyeD3Tag
plaintext α = string α $> Empty

-- The whole thing
-- Compiles, but not quite accurate with delimeters
formatString ∷ Stream s m Char ⇒ ParsecT s u m (ParsecT s u m EyeD3Tag)
formatString = (foldr1 (◇)) ⊙ (many1 $ tag' <|> exactText')
