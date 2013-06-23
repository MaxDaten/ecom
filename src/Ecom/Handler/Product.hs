{-# LANGUAGE TupleSections, OverloadedStrings, RecordWildCards #-}
module Ecom.Handler.Product where

import Ecom.Import
import Ecom.Utils

import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (pack, unpack)
import qualified Data.UUID as UUID
import Data.Maybe

import Data.Colour.SRGB (sRGB24show, sRGB24read, toSRGB)
import Data.Colour (darken)


getProductR :: ProductId -> Handler RepHtml
getProductR pid = do
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


-- case case case case ....  i want to use the maybe monad, 
-- but i dont know how to inject a propper error handling
-- arrows? mzero? applicative?
postBuyProductR :: ProductId -> Handler RepHtml
postBuyProductR baseProductId = do
    mUser <- lookupSession "name"
    mProduct <- acidQuery (ProductById baseProductId)

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
                            ((result, widget), enctype) <- runFormPost (productBuyForm product)
                            case result of
                                FormSuccess product -> do
                                    acidUpdate (AddProductToUserHistory product user)
                                    setInfoMessageI (MsgProductBought product)
                                _ -> setErrorMessageI MsgInvalidInput
                            redirect (ProductR baseProductId)


productBuyAForm :: Product -> AForm Handler Product
productBuyAForm baseProduct = specificVariant 
    <$> areq (sizesField baseProduct) "MsgProductSizeAttribName" Nothing
    <*> areq (colorField baseProduct) "MsgProductColorAttribName" Nothing
    where
        specificVariant size color = Product
            (productId baseProduct) 
            (productTitle baseProduct)
            (productCategories baseProduct)
            (Set.singleton size)
            (Set.singleton color)
            (productDescription baseProduct)


sizesField :: Product -> Field Handler ProductSize
sizesField = selectField . return . mkOptionList . (map mkSizeOption) . Set.toList . productSizes


colorField :: Product -> Field Handler ProductColor
colorField = radioField . return . mkOptionList . (map mkColorOption) . Set.toList . productColors


productBuyForm :: Product -> Html -> MForm Handler (FormResult Product, Widget)
productBuyForm = renderDivs . productBuyAForm

-- ^{colorPreview color 50 50}
basicBuyForm :: ProductId -> Widget -> Enctype -> WidgetT Ecom IO ()
basicBuyForm pid widget enctype = toWidget $
    [whamlet|
    <form .pull-right method=post action=@{BuyProductR pid} enctype=#{enctype}>
        ^{widget}
        <button .btn-primary .btn-large .btn-buy .btn>
           <i class="icon-shopping-cart icon-white"></i> _{MsgBuyProduct} 
    |]

mkColorOption :: ProductColor -> Option ProductColor
mkColorOption pcolor@(ProductColor c) = Option 
    { optionDisplay = pack.sRGB24show.unProductColor $ pcolor
    , optionInternalValue = pcolor
    , optionExternalValue = pack.sRGB24show.unProductColor $ pcolor
    }

mkSizeOption :: ProductSize -> Option ProductSize
mkSizeOption psize@(ProductSize s) = Option
    { optionDisplay = pack.show $ s
    , optionInternalValue = psize
    , optionExternalValue = pack.show $ s
    }

