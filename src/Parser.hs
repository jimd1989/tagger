module Parser (FileParser(..), fileParser) where

import Prelude (Char, Eq, Int, Show, String, (.), (-), ($), (==), (>>=), show)
import Control.Applicative ((<|>), (*>), (<*), liftA2, pure)
import Control.Arrow ((+++))
import Data.Attoparsec.Combinator (choice, count, lookAhead, many1,
                                   manyTill, option, try)
import Data.Attoparsec.Text (anyChar, char, digit, parseOnly, string)
import Data.Attoparsec.Types (Parser)
import Data.Bitraversable (bisequence)
import Data.Either (Either)
import Data.Functor (($>))
import Data.List (filter, foldl1, foldr1, intercalate, length, zipWith)
import Data.Maybe (Maybe(..))
import Data.Text (Text, pack)
import Data.Tuple (uncurry)
import EyeD3Tag (EyeD3Tag(..), Tagger(..), getTag)
import Helpers ((⊙), (◇))

newtype Delimeter = Delimeter { getDelimeter ∷ Maybe Char }

newtype FileParser = FileParser { runParser ∷ Text → Either String Text }

-- Helpers
oneOf ∷ String → Parser Text Char
oneOf = foldl1 (<|>) . (char ⊙)

nextChar ∷ Parser Text Char
nextChar = anyChar *> anyChar

untilChar ∷ Char → Parser Text String
untilChar α = try $ manyTill anyChar (lookAhead $ char α)

includingChar ∷ Char → Parser Text String
includingChar α = liftA2 snoc (untilChar α) anyChar
  where snoc α ω = α ◇ [ω]

untilEof ∷ Parser Text String
untilEof = many1 anyChar

delimeter ∷ Parser Text Delimeter
delimeter = Delimeter ⊙ optionMaybe (try $ lookAhead nextChar)
  where optionMaybe α = option Nothing (Just ⊙ α)

delimeterCount ∷ Delimeter → Parser Text Int
delimeterCount α = case (getDelimeter α) of
  (Just  ω) → (length . (filter (== ω))) ⊙ lookAhead untilEof
  (Nothing) → pure 0

delimeterAndCount ∷ Parser Text (Delimeter, Int)
delimeterAndCount = bisequence (delimeter, delimeter >>= delimeterCount)

tagger ∷ Char → (Text → EyeD3Tag) → Parser Text Tagger
tagger α ω = char α $> Tagger ω

-- Format string parsers
exactText ∷ Parser Text (Parser Text Text)
exactText = plaintext ⊙ (untilChar '{' <|> untilEof)

textTag ∷ Parser Text (Parser Text Text)
textTag = check *> liftA2 (uncurry . text) choices delimeterAndCount
  where chars        = "abAGt"
        constructors = [Artist, AlbumArtist, Album, Genre, Title]
        check        = lookAhead $ oneOf chars
        choices      = choice $ zipWith tagger chars constructors

numTag ∷ Parser Text (Parser Text Text)
numTag = number ⊙ (check *> choices)
  where chars        = "dnY"
        constructors = [DiscNum, TrackNum, Year]
        check        = lookAhead $ oneOf chars
        choices      = choice $ zipWith tagger chars constructors

tag ∷ Parser Text (Parser Text Text)
tag = (char '{') *> (numTag <|> textTag) <* (char '}')

-- Generated file name parsers
untilDelimeter ∷ Int → Delimeter → Parser Text String
untilDelimeter n α = case (getDelimeter α) of
  (Just  ω) → liftA2 (◇) (cat ⊙ (count n $ includingChar ω)) (untilChar ω)
  (Nothing) → untilEof
  where cat = intercalate ""

plaintext ∷ String → Parser Text Text
plaintext α = string (pack α) $> ""

text ∷ Tagger → Delimeter → Int → Parser Text Text
text f α n = (getTag f . pack) ⊙ (delimeterCount α >>= nFields)
  where nFields ω = untilDelimeter (ω - n) α

number ∷ Tagger → Parser Text Text
number f = (getTag f . pack) ⊙ (many1 digit)

-- The whole thing
formatString ∷ Parser Text (Parser Text Text)
formatString = (foldr1 (◇)) ⊙ (many1 $ tag <|> exactText)

fileParser ∷ Text → Either String FileParser
fileParser = (show +++ (FileParser . parseOnly)) . parseOnly formatString
