
{-# LANGUAGE OverloadedStrings #-}

module Html (indexHtml, booksHtml) where



import           Prelude                     hiding (div, head, id)

import           Control.Monad               (forM_)
import           Data.Monoid
import           Data.Text                   hiding (head)
import           Data.Time.Calendar
import           Text.Blaze.Html5            hiding (main, map)
import           Text.Blaze.Html5.Attributes hiding (form, label, title)
import qualified Types                       as T

--import           Text.Blaze.Html.Renderer.Pretty        (renderHtml)

--appName = "ideal-pancake"

bootstrapUrl :: AttributeValue
bootstrapUrl = "https://" <> domain <> "/bootstrap/"
                          <> version <> "/css/" <> file
  where domain = "maxcdn.bootstrapcdn.com"
        version = "3.3.7"
        file = "bootstrap.min.css"

bootstrap :: Html
bootstrap = link
  ! rel "stylesheet"
  ! href bootstrapUrl
  ! crossorigin "anonymous"

headerHtml :: Html
headerHtml =
  head $ do
    meta ! charset "utf-8"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    title $ text "Ideal Pancake"
    bootstrap

indexHtml :: Html
indexHtml = do
  headerHtml
  body $ do
    container $ do
      row $ text "Welcome to ideal-pancake"
      row $
        form $ do
          formGroup $ do
            label ! for "email" $ text "Email address:"
            input ! type_ "email" ! name "email"
          formGroup $ do
            label ! for "password" $ text "Password:"
            input ! type_ "password" ! name "password"
          formGroup $
            label $ do
              input ! type_ "checkbox"
              text "Remember me"
          button ! type_ "submit" ! class_ "btn btn-default" $ text "Login"
    booksHtml []

booksHtml :: [T.Book] -> Html
booksHtml books = do
  headerHtml
  body $ do
    div ! class_ "contaienr hidden-xs hidden-sm" $ do
      colMd 3 . b $ text "Title"
      colMd 3 . b $ text "Authors"
      colMd 2 . b $ text "Publishers"
      colMd 2 . b $ text "Year"
      colMd 2 . b $ text "ISBN"
    books `forM_` eachBook
    where
      eachBook :: T.Book -> Html
      eachBook (T.Book isbn bookTitle authors publishers year) = do
        colMd 3 . text   $ bookTitle
        colMd 3 . rowsOf $ authors
        colMd 2 . rowsOf $ publishers
        colMd 2 . text   $ showYear year
        colMd 2 . string $ show isbn

colMd :: Int -> Html -> Html
colMd n = div ! class_ (stringValue ("col-md-" <> show n))

{-
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
-}

-- rowsHtml :: (Foldable t) => t Html -> Html
-- rowsHtml as = as `forM_` row

rowsOf :: (Foldable t) => t Text -> Html
rowsOf as =
  container $
    as `forM_` (row . text)

container :: Html -> Html
container = div ! class_ "container"

row :: Html -> Html
row = div ! class_ "row"

formGroup :: Html -> Html
formGroup = div ! class_ "form-group"

crossorigin :: AttributeValue -> Attribute
crossorigin = customAttribute "crossorigin"

showYear :: Day -> Text
showYear d =
  let (yr, _, _) = toGregorian d
  in  pack $ show yr
