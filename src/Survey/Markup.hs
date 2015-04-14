module Survey.Markup where

import Survey.Types
import Survey.Utilities

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Map (Map)
import Data.Monoid
import Data.Text (Text)
import Lucid
import Lucid.Html5
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

homepage :: [FieldName] -> HTML
homepage fields = withHeader $ do
  form_ [action_ "/rows"] $ do
    ul_ $ do
      forM_ fields $ \field -> do
        li_ $ do
          input_ [type_ "checkbox", id_ field, name_ field]
          label_ [for_ field] $ toHtml field
      input_ [type_ "checkbox", checked_, id_ "all", name_ "all"]
      label_ [for_ "all"] $ "only show responses where every row has been filled"
      input_ [type_ "submit"]

showRows :: Bool -> [FieldName] -> HtmlT (ReaderT CustomCSV Maybe) ()
showRows onlyAll rows = withHeader $ do
  (_, ms) <- lift ask
  allEntries <- do
    es <-
      forM ms $ \m -> do
        forM rows $ \rowName -> do
          e <- lift $ lift $ Map.lookup rowName m
          return (rowName, e)
    let f = if onlyAll then all else any
    return $ filter (f (not . Text.null . snd)) es
  toHtml $ "there are " <> tshow (length allEntries) <> " filled-in responses"
  forM_ (allEntries `zip` [1..]) $ \(entries, n) -> do
    ul_ [id_ (tshow n)] $ do
      a_ [href_ ("#" <> tshow n)] $ "#"
      " "
      a_ [href_ ("/row/" <> tshow n)] $ "view entire response"
      forM_ entries $ \(rowName, e) -> do
        li_ $ do
          div_ [class_ "row_name"] $ toHtml rowName
          toHtml e

showRow :: [FieldName] -> Map Text Text -> HTML
showRow hs m = withHeader $ do
  ul_ $ do
    forM_ hs $ \h -> do
      case Map.lookup h m of
        Just x -> li_ $ do
          div_ [class_ "row_name"] $ toHtml h
          toHtml x
        Nothing -> return ()

withHeader :: Monad m => HtmlT m a -> HtmlT m a
withHeader content = do
  doctype_
  head_ $ do
    title_ "survey"
    link_ [type_ "text/css", rel_ "stylesheet", href_ "/style.css"]
  body_ $ do
    content
