{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Ecom.Handler.Home where

import Ecom.Import
import Data.Text hiding (null)

getHomeR :: Handler RepHtml
getHomeR = do
    allProducts <- acidQuery (AllProducts)
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

