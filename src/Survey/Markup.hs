module Survey.Markup where

import Survey.Types
import Survey.Utilities

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Monoid
import Lucid
import Lucid.Html5
import qualified Data.Map as Map
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text

homepage :: [FieldName] -> HTML
homepage fields = withHeader $ do
  form_ [action_ "/rows"] $ do
    ul_ $ do
      forM_ fields $ \field -> do
        li_ $ do
          input_ [type_ "checkbox", id_ field, name_ field]
          label_ [for_ field] $ toHtml field
      input_ [type_ "submit"]

showRows :: [FieldName] -> HtmlT (ReaderT CustomCSV Maybe) ()
showRows rows = withHeader $ do
  (_, ms) <- lift ask
  allEntries <- do
    es <-
      forM ms $ \m -> do
        forM rows $ \rowName -> do
          e <- lift $ lift $ Map.lookup rowName m
          return (rowName, e)
    return $ filter (all (not . Text.null . snd)) es
  toHtml $ "there are " <> tshow (length allEntries) <> " filled-in responses"
  forM_ (allEntries `zip` [1..]) $ \(entries, n) -> do
    ul_ [id_ (tshow n)] $ do
      a_ [href_ ("#" <> tshow n)] $ "#"
      forM_ entries $ \(rowName, e) -> do
        li_ $ do
          div_ [class_ "row_name"] $ toHtml rowName
          toHtml e

withHeader :: Monad m => HtmlT m a -> HtmlT m a
withHeader content = do
  doctype_
  head_ $ do
    title_ "survey"
    link_ [type_ "text/css", rel_ "stylesheet", href_ "/style.css"]
  body_ $ do
    content
