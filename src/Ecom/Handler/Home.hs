{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Ecom.Handler.Home where

import Ecom.Import

getHomeR :: Handler RepHtml
getHomeR = do
    allProducts <- acidQuery (AllProducts)
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Willkommen!"
        $(widgetFile "homepage")

