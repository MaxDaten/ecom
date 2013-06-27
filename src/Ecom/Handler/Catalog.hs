{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Ecom.Handler.Catalog where


import Ecom.Import
import Ecom.Utils
import Control.Monad (join, unless)
import Data.Maybe (maybeToList)
import Text.Julius (rawJS)

getCatalogR :: Handler RepHtml
getCatalogR = redirect $ CatalogThresholdR 30


getCatalogThresholdR :: Int -> Handler RepHtml
getCatalogThresholdR th = do
    allProducts <- acidQuery (AllProducts)
    prodImg <- widgetToPageContent $ placeholditWidget 250 210

    mUser <- fuser =<< lookupSession "name"
    let userHistory    = join . maybeToList $ history <$> mUser
    recommendedProducts <- maybe (return []) (\user -> acidQuery (SimilarProducts userHistory (attributes user) (realToFrac th))) mUser

    let thresholdDim = (5, 400) :: (Double, Double)
    let startValue = th

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