
module Html where

{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (head, id, div)

import           Data.Monoid
import           Data.Text                       hiding (head)
import qualified Data.ByteString.Lazy.Char8      as L
import           Data.Time
import           Text.Blaze.Html5                hiding (map, main)
import           Text.Blaze.Html5.Attributes     hiding (title, form, label)
import           Text.Blaze.Html.Renderer.Pretty        (renderHtml)


booksHtml :: [Book] -> Html
booksHtml bks = do
  table ! class_ "table-responsive" $ do
    thead $ do 
      tr $ do
        th $ text "Title"
        th $ text "Authors"
        th $ text "Publishers"
        th $ text "Year"
        th $ text "ISBN"
    tbody $ do
      bks `forM_` book
  where
    book (Book isbn title authors publishers year) = do
      tr $ do
        td . text   $ title
        td . rowsOf $ authors
        td . rowsOf $ publishers
        td . showYear year
        td . text   $ isbn
          
rowsOf as = do
  container $ do
    as `forM_` (row . text)
          
container :: Html -> Html
container = div ! class_ "container"

row :: Html -> Html
row = div ! class_ "row"

formGroup :: Html -> Html
formGroup = div ! class_ "form-group"

crossorigin = customAttribute "crossorigin"

showYear d =
  let (yr, _, _) = toGregorian . utctDay d
  in  pack $ show yr
