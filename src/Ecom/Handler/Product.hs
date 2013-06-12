{-# LANGUAGE TupleSections, OverloadedStrings, RecordWildCards #-}
module Ecom.Handler.Product where

import Ecom.Import
import Ecom.Utils

import Data.Colour (darken)

getProductR :: ProductId -> Handler RepHtml
getProductR pid = do
    mProduct <- acidQuery (ProductById pid)

    case mProduct of
        Nothing                     -> notFound
        Just (product)  -> do
            assocedProducts <- acidQuery (AssociatedProducts product)
            mainImg <- widgetToPageContent $ placeholditWidget 500 500
            relImg <- widgetToPageContent $ placeholditWidget 100 100
            defaultLayout $ do
                setTitle $ toHtml $ getProductTitle product
                $(widgetFile "product")
                $(widgetFile "productAssoc")


--productWidget :: Product ->
