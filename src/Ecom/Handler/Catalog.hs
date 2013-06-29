{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-name-shadowing -fno-warn-hi-shadowing #-}
module Ecom.Handler.Catalog where


import Ecom.Import
import Ecom.Utils
import Ecom.Forms
import Control.Monad (join, unless)
import Data.Maybe (maybeToList)


handleProductRecsRootR :: Handler RepHtml
handleProductRecsRootR = redirect (CatalogR)

handleProductPidRootR :: Handler RepHtml
handleProductPidRootR = redirect (CatalogR)

getCatalogR :: Handler RepHtml
getCatalogR = do
    allProducts <- acidQuery (AllProducts)
    prodImg <- widgetToPageContent $ placeholditWidget 250 210

    mUser <- getUser =<< lookupSession "name"
    let userHistory    = join . maybeToList $ history <$> mUser

    let th = 30 :: Int-- !!!
    recommendedProducts <- maybe (return []) (\user -> acidQuery (SimilarProducts userHistory (attributes user) (realToFrac th))) mUser

    let thresholdDim = (5, 400) :: (Double, Double)
    let startValue = th

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Willkommen!"
        $(widgetFile "homepage")
        unless (null recommendedProducts) $ do
            $(widgetFile "rec-products")
        $(widgetFile "catalog")


getProductPidR :: ProductId -> Handler RepHtml
getProductPidR pid = do
    mProduct <- acidQuery (ProductById pid)

    case mProduct of
        Nothing         -> notFound
        Just product    -> do
            assocedProducts   <- acidQuery (AssociatedProducts product)
            mainImg           <- widgetToPageContent $ placeholditWidget 500 500
            relImg            <- widgetToPageContent $ placeholditWidget 100 100
            (formWidget, enc) <- generateFormPost $ productBuyForm product
            defaultLayout $ do
                setTitle $ toHtml $ getProductTitle product
                $(widgetFile "product")
                $(widgetFile "productAssoc")


getProductRecsR :: Int -> Handler TypedContent
getProductRecsR threshold = do
    mUser <- getUser =<< lookupSession "name"
    let userHistory = join . maybeToList $ history <$> mUser

    prodImg <- widgetToPageContent $ placeholditWidget 250 210

    recommendedProducts <- maybe (return []) (\user -> acidQuery (SimilarProducts userHistory (attributes user) (realToFrac threshold))) mUser
    let thresholdDim = (5, 400) :: (Double, Double)
    let startValue = threshold
    let widget = $(widgetFile "rec-products")
    defaultLayoutJson widget (return recommendedProducts)


getUser :: Maybe Text -> Handler (Maybe User) 
getUser = maybe (return Nothing) (\u -> acidQuery (UserByName u))




-- case case case case ....  i want to use the maybe monad, 
-- but i dont know how to inject a propper error handling
-- arrows? mzero? applicative?
postBuyProductR :: ProductId -> Handler RepHtml
postBuyProductR baseProductId = do
    mUser <- lookupSession "name"

    case mUser of
        Nothing -> setErrorMessageI MsgPleaseLogin >> redirect HomeR
        Just (username) -> do
            mUser' <- acidQuery (UserByName username)

            case mUser' of
                Nothing -> setErrorMessageI MsgInvalidUser >> notAuthenticated
                Just user -> do
                    
                    mProduct <- acidQuery (ProductById baseProductId)
                    case mProduct of
                        Nothing -> setErrorMessageI MsgProductNotFound >> notFound
                        Just product -> do
                            ((result, _), _) <- runFormPost (productBuyForm product)
                            case result of
                                FormSuccess product -> do
                                    _ <- acidUpdate (AddProductToUserHistory product user)
                                    setInfoMessageI (MsgProductBought product)
                                _ -> setErrorMessageI MsgInvalidInput
                            redirect (ProductPidR baseProductId)
