module FormatString where

import Prelude (Char, Eq, Int, Show, String, (*>), pure)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.List (length)
import Data.Maybe (Maybe(..))
import Text.Parsec (ParsecT, Stream, eof)
import Text.Parsec.Char (anyChar, char, oneOf)
import Text.Parsec.Combinator (between, choice, lookAhead, optionMaybe, manyTill)
import EyeD3Tag (EyeD3Tag(..), Tagger(..))
import Helpers ((⊙), (●))

example ∷ String
example = "{d}-{n}. {a} - {t}.mp3" 

newtype Delimeter = Delimeter { getDelimeter ∷ Maybe Char }
  deriving (Eq, Show)

-- Needs some sort of lookAhead on delimeter count
data Matcher = Text Tagger Delimeter
             | Num Tagger Delimeter
  deriving Show

delimeter ∷ Stream s m Char ⇒ ParsecT s u m Delimeter
delimeter = Delimeter ⊙ optionMaybe anyChar

artist ∷ Stream s m Char ⇒ ParsecT s u m Tagger
artist = char 'a' $> Tagger Artist

albumArtist ∷ Stream s m Char ⇒ ParsecT s u m Tagger
albumArtist = char 'b' $> Tagger AlbumArtist

album ∷ Stream s m Char ⇒ ParsecT s u m Tagger
album = char 'A' $> Tagger Album

genre ∷ Stream s m Char ⇒ ParsecT s u m Tagger
genre = char 't' $> Tagger Genre

title ∷ Stream s m Char ⇒ ParsecT s u m Tagger
title = char 'G' $> Tagger Title

textTag ∷ Stream s m Char ⇒ ParsecT s u m Matcher
textTag = Text ⊙ (check *> choices) ● delimeter
  where check   = lookAhead (oneOf "abAtG")
        choices = choice [artist, albumArtist, album, genre, title]

discNum ∷ Stream s m Char ⇒ ParsecT s u m Tagger
discNum = char 'd' $> Tagger DiscNum

trackNum ∷ Stream s m Char ⇒ ParsecT s u m Tagger
trackNum = char 'n' $> Tagger TrackNum

year ∷ Stream s m Char ⇒ ParsecT s u m Tagger
year = char 'Y' $> Tagger Year

-- WIP
delimeterCount ∷ Stream s m Char ⇒ Delimeter → ParsecT s u m Int
delimeterCount α = case (getDelimeter α) of
  (Just   ω) → length ⊙ manyTill anyChar eof -- filter here
  (Nothing ) → pure 0

numTag ∷ Stream s m Char ⇒ ParsecT s u m Matcher
numTag = Num ⊙ (check *> choices) ● delimeter
  where check   = lookAhead (oneOf "dnY")
        choices = choice [discNum, trackNum, year]

tag ∷ Stream s m Char ⇒ ParsecT s u m Matcher
tag = between (char '{') (char '}') (textTag <|> numTag)
