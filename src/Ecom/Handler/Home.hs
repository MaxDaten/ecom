{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Ecom.Handler.Home where

import Ecom.Import
import Ecom.Utils

getHomeR :: Handler RepHtml
getHomeR = do
    allProducts <- acidQuery (AllProducts)
    prodImg <- widgetToPageContent $ placeholditWidget 250 210
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Willkommen!"
        $(widgetFile "homepage")

