module Survey.Utilities where

import Survey.Types

import Data.ByteString (ByteString)
import Data.Csv
import Data.Map (Map)
import Data.Text (Text)
import Lucid
import System.Exit
import Web.Scotty.Trans
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Vector as Vector
import qualified Data.Map.Strict as Map

lucid :: HTML -> ActionM ()
lucid = html . renderText

fromLazy :: Lazy.Text -> Text.Text
fromLazy = Lazy.toStrict

toLazy :: Text.Text -> Lazy.Text
toLazy = Lazy.fromStrict

readCSV :: IO ([Text], [Map Text Text])
readCSV = do
  file <- ByteString.readFile "Survey Responses.csv"
  case decodeByName file of
    Left err -> do
      putStrLn err
      exitFailure
    Right (hdr, resps) -> do
      let dec = Text.decodeUtf8
      let hdr' = fmap dec $ Vector.toList hdr
      let resps' = fmap (Map.mapKeysMonotonic dec . fmap dec) $ Vector.toList resps
      return (hdr', resps')

tshow :: Show a => a -> Text
tshow = Text.pack . show
