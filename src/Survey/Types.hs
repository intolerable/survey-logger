module Survey.Types where

import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text)
import Lucid
import Web.Scotty.Trans
import qualified Data.Text.Lazy as Lazy

type FieldName = Text

type HTML = Html ()

type CustomCSV = ([Text], [Map Text Text])

type ActionM = ActionT Lazy.Text (ReaderT CustomCSV IO)
