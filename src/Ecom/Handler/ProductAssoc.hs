{-# LANGUAGE TupleSections, OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Ecom.Handler.ProductAssoc where

import Ecom.Import
import Ecom.Utils

getProductAssocR :: ProductId -> Handler RepHtml
getProductAssocR pid = do
  mProduct         <- acidQuery (ProductById pid)
  case mProduct of
    Nothing -> notFound
    Just p  -> do
        assocedProducts <- acidQuery (AssociatedProducts p)
        let (relImgWidth, relImgHeight) = (show (100 :: Int), show (100 :: Int))
        relImg <- widgetToPageContent $ placeholditWidget 100 100
        defaultLayout $ do
            setTitle $ toHtml (show $ productTitle p)
            $(widgetFile "productAssoc")
