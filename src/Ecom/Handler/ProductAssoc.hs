{-# LANGUAGE TupleSections, OverloadedStrings, RecordWildCards #-}
module Ecom.Handler.ProductAssoc where

import Ecom.Import

getProductAssocR :: ProductId -> Handler RepHtml
getProductAssocR pid = do
  product         <- acidQuery (ProductById pid)
  case product of
    Nothing -> notFound
    Just p  -> do
                  assocedProducts <- acidQuery (AssociatedProducts p)
                  defaultLayout $ do
                                    setTitle $ toHtml (show $ productTitle p)
                                    $(widgetFile "productAssoc")