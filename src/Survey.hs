module Survey where

import Survey.Markup
import Survey.Stylesheet
import Survey.Types
import Survey.Utilities

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Text.Encoding as Text
import Lucid
import Web.Scotty.Trans
import qualified Data.Map as Map
import qualified Data.Text.Lazy as Lazy
import Stitch.Render
import Network.Wai.Middleware.RequestLogger

main :: IO ()
main = do
  csv <- readCSV
  scottyT 3000 (`runReaderT` csv) (`runReaderT` csv) $ do
    middleware logStdoutDev
    get "/" $ do
      (header, _) <- lift ask
      lucid $ homepage header
    get "/rows" $ do
      rows <- params
      c <- lift ask
      let x = renderTextT $ showRows $ map (fromLazy . fst) $ filter ((=="on") . snd) rows
      case runReaderT x c of
        Just x -> html x
        Nothing -> next
    get "/style.css" $ do
      setHeader "Content-Type" "text/css, charset=utf-8"
      text $ toLazy $ renderCSSWith compressed style

