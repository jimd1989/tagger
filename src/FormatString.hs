module FormatString (formatString) where

-- Takes a string representing the tag format of the filenames to be tagged.
-- Example: "{d}-{n}. {a} - {t}.mp3"
-- Returns a list of Matchers to handle the tag parsing of each filename.

import Prelude (Char, Eq, Int, String, (.), ($), (*>), (==), (>>=))
import Control.Applicative ((<|>), liftA2, pure)
import Data.Bitraversable (bisequence)
import Data.Functor (($>))
import Data.List (filter, length, zipWith)
import Data.Maybe (Maybe(..))
import Data.Tuple (uncurry)
import Text.Parsec (ParsecT, Stream, eof, many1, manyTill, try)
import Text.Parsec.Char (anyChar, char, oneOf)
import Text.Parsec.Combinator (between, choice, lookAhead, optionMaybe)
import EyeD3Tag (EyeD3Tag(..), Tagger(..))
import Helpers ((⊙))
import Matcher (Delimeter(..), Matcher(..))

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

exactText ∷ Stream s m Char ⇒ ParsecT s u m Matcher
exactText = ExactText ⊙ (openBracket <|> end)
  where openBracket = try $ manyTill anyChar (lookAhead $ char '{')
        end         = many1 anyChar

formatString ∷ Stream s m Char ⇒ ParsecT s u m [Matcher]
formatString = many1 $ tag <|> exactText
