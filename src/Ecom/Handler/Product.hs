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


getBuyProductR :: ProductId -> Handler RepHtml
getBuyProductR pid = do
    mUser <- lookupSession "name"
    case mUser of
        Just username -> do
            mUser' <- acidQuery (UserByName username)
            
            case mUser' of
                Nothing -> do
                    setErrorMessageI MsgInvalidUser
                    notAuthenticated
                Just user -> do
                    mProd <- acidQuery (ProductById pid)

                    case mProd of
                        Nothing -> notFound
                        Just product -> do
                                setInfoMessageI (MsgProductBought product)
                                acidUpdate (AddProductToUserHistory product user)
                                redirect $ ProductR pid
        Nothing -> do
            setErrorMessageI MsgPleaseLogin
            notAuthenticated
 
--productWidget :: Product ->
