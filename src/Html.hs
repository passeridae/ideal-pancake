
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
