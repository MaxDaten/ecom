{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Ecom.Handler.Home where

import Ecom.Import
import Ecom.Utils
import Control.Monad (join, unless)
import Data.Maybe (maybeToList)

getHomeR :: Handler RepHtml
getHomeR = do
    allProducts <- acidQuery (AllProducts)
    prodImg <- widgetToPageContent $ placeholditWidget 250 210

    mUser <- fuser =<< lookupSession "name"
    let userHistory = join . maybeToList $ history <$> mUser
    recommendedProducts <- acidQuery (SimilarProducts userHistory 5.0)

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Willkommen!"
        $(widgetFile "homepage")
        unless (null recommendedProducts) 
            $(widgetFile "recommended-products")
        $(widgetFile "product-catalog")
    where
        fuser :: Maybe Text -> Handler (Maybe User) 
        fuser Nothing = return Nothing
        fuser (Just u) = acidQuery (UserByName u)

