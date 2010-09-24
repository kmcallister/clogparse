import Data.Word
import Control.Applicative
import Data.List
import System
import System.IO.Unsafe(unsafePerformIO)
import Data.Maybe

import qualified Data.Foldable as F
import qualified Data.Attoparsec as P
import qualified Data.Attoparsec.FastSet as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Time as Time
import qualified Data.Time.LocalTime.TimeZone.Series as Zone
import qualified Data.Time.LocalTime.TimeZone.Olson  as Zone
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

import qualified Text.Groom as Groom

logZone :: String
logZone = "/usr/share/zoneinfo/America/Los_Angeles"

type TimeConv = Time.LocalTime -> Time.UTCTime

getTimeConv :: IO TimeConv
getTimeConv = Zone.localTimeToUTC' <$> Zone.getTimeZoneSeriesFromOlsonFile logZone


-- People use many text encodings on IRC.  So we parse metadata from bytes
-- (it's ASCII), then try to decode messages.

atoi :: (Integral a, Num b) => [a] -> b
atoi = foldl' (\n d -> n*10 + fromIntegral d - 48) 0

digits :: Int -> P.Parser Int
digits n = atoi <$> P.count n digit where
  digit = P.satisfy isDigit
  isDigit w = w >= 48 && w <= 57

time :: P.Parser Time.DiffTime
time = f <$> digits 2 <* colon <*> digits 2 <* colon <*> digits 2 where
  -- FIXME: leap seconds
  f h m s = Time.secondsToDiffTime $ fromIntegral (3600*h + 60*m + s)
  colon = P.word8 58

data EventAt
  = EventAt  Time.DiffTime Event
  | BadParse T.Text
  deriving (Show)

type Nick = T.Text

data Event
  = Join   Nick      T.Text
  | Part   Nick      T.Text
  | Quit   Nick      T.Text
  | Nick   Nick Nick
  | Talk   Nick      T.Text
  | Notice Nick      T.Text
  | Act    Nick      T.Text
  | Kick   Nick Nick T.Text
  | Mode   Nick      T.Text
  | Log              T.Text
  | Topic            T.Text
  | Names            T.Text
  deriving (Show)

notNewline :: Word8 -> Bool
notNewline w = w /= 13 && w /= 10

decode :: B.ByteString -> T.Text
decode = T.decodeUtf8With T.lenientDecode

restOfLine :: P.Parser T.Text
restOfLine = decode <$> P.takeWhile notNewline <* P.take 1

nextLine :: P.Parser ()
nextLine = P.skipWhile notNewline <* P.take 1

event :: P.Parser Event
event = F.asum
  [ str " --- " *> F.asum
    [ userAct Join "join: "
    , userAct Part "part: "
    , userAct Quit "quit: "
    , Nick <$ str "nick: " <*> nick <* str " -> " <*> nick <* nextLine
    , Mode <$ str "mode: " <*> nick <* str " set " <*> restOfLine
    , Kick <$ str "kick: " <*> nick <* str " was kicked by " <*> nick <* chr ' ' <*> restOfLine
    , global Log   "log: "
    , global Topic "topic: "
    , global Names "names: "
    ]
  , Talk   <$ str " <" <*> nick <* chr '>' <*> restOfLine
  , Notice <$ str " -" <*> nick <*> restOfLine -- FIXME: parse host
  , Act    <$ str " *" <*> nick <*> restOfLine
  ] where
    chr  = P.word8  . fromIntegral . fromEnum
    str  = P.string . B8.pack
    noNickC  = P.charClass " \n\r\t\v<>"
    nick = decode <$> P.takeWhile (not . flip P.memberWord8 noNickC)
    userAct f x = f <$ str x <*> nick <* chr ' ' <*> restOfLine
    global f x = f <$ str x <*> restOfLine

eventAt :: P.Parser EventAt
eventAt = EventAt <$> time <*> event

warn :: String -> a -> a
warn xs a = unsafePerformIO (putStrLn xs >> return a)

line :: P.Parser EventAt
line =
  P.try (EventAt  <$> time <*> event)
  <|>   (BadParse <$> restOfLine)

parseFile :: FilePath -> IO [EventAt]
parseFile p = do
  b <- B.readFile p
  let go r@P.Fail{}    = error $ show r
      go (P.Partial g) = go $ g B.empty
      go (P.Done _  x) = x
  let es = go $ P.parse (P.manyTill line P.endOfInput) b
  return es

main :: IO ()
main = do
  getArgs >>= (mapM_ $ \c -> do
    xs  <- parseFile c
    --putStrLn $ Groom.groom xs
    print (c, [ x | BadParse x <- xs ]))
