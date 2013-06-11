{-# LANGUAGE TupleSections, OverloadedStrings, RecordWildCards #-}
module Ecom.Handler.Product where

import Ecom.Import

getProductR :: ProductId -> Handler RepHtml
getProductR pid = do
	mProduct <- acidQuery (ProductById pid)

	case mProduct of
		Nothing 	 				-> notFound
		Just (product@Product{..}) 	-> defaultLayout $ do
			setTitle $ toHtml (show productTitle)
			$(widgetFile "product")
