
{-# LANGUAGE OverloadedStrings #-}

module Html (indexHtml, booksHtml) where



import Prelude hiding (head, id, div)

import qualified Types as T
import           Control.Monad (forM_)
import           Data.Text                       hiding (head)
import           Data.Time.Calendar
import           Text.Blaze.Html5                hiding (map, main)
import           Text.Blaze.Html5.Attributes     hiding (title, form, label)
--import           Text.Blaze.Html.Renderer.Pretty        (renderHtml)


headerHtml :: Html
headerHtml = do
  head $ do
    meta ! charset "utf-8"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    title $ text "Ideal Pancake"
    link
      ! rel  "stylesheet"
      ! href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" 

  

indexHtml :: Html
indexHtml = do
  headerHtml
  body $ do
    container $ do
      row $ text "Welcome to ideal-pancake"
      row $ do
        form $ do
          formGroup $ do
            label ! for "email" $ text "Email address:"
            input ! type_ "email" ! name "email"
          formGroup $ do
            label ! for "password" $ text "Password:"
            input ! type_ "password" ! name "password"
          formGroup $ do
            label $ do
              input ! type_ "checkbox"
              text "Remember me"
          button ! type_ "submit" ! class_ "btn btn-default" $ text "Login"
    booksHtml []
    
booksHtml :: [T.Book] -> Html
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
    book (T.Book isbn bookTitle authors publishers year) = do
      tr $ do
        td . text   $ bookTitle
        td . rowsOf $ authors
        td . rowsOf $ publishers
        td . text   $ showYear year
        td . string $ show isbn
          
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

showYear :: Day -> Text
showYear d =
  let (yr, _, _) = toGregorian d
  in  pack $ show yr
