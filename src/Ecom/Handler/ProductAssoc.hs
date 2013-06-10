{-# LANGUAGE TupleSections, OverloadedStrings, RecordWildCards #-}
module Ecom.Handler.ProductAssoc where

import Ecom.Import

getProductAssocR :: ProductId -> Handler RepHtml
getProductAssocR pid = do
	mProduct <- acidQuery (ProductById pid)

	case mProduct of
		Nothing 	 				-> notFound
		Just (product@Product{..}) 	-> defaultLayout $ do
			setTitle $ toHtml (show productTitle)
			$(widgetFile "productAssoc")