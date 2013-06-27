{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-name-shadowing -fno-warn-hi-shadowing #-}
module Ecom.Handler.Catalog where


import Ecom.Import
import Ecom.Utils
import Control.Monad (join, unless)
import Data.Maybe (maybeToList)
import Text.Julius (rawJS)

import qualified Data.Set as Set

import Data.Text (pack)

import Data.Colour.SRGB (sRGB24show)


getCatalogR :: Handler RepHtml
getCatalogR = do
    allProducts <- acidQuery (AllProducts)
    prodImg <- widgetToPageContent $ placeholditWidget 250 210

    mUser <- fuser =<< lookupSession "name"
    let userHistory    = join . maybeToList $ history <$> mUser

    let th = 30 :: Int-- !!!
    recommendedProducts <- maybe (return []) (\user -> acidQuery (SimilarProducts userHistory (attributes user) (realToFrac th))) mUser

    r <- getCurrentRoute
    rf <- getUrlRender
    let currentR = maybe ("" :: Text) rf r

    let thresholdDim = (5, 400) :: (Double, Double)
    let startValue = th

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Willkommen!"
        $(widgetFile "homepage")
        unless (null recommendedProducts) 
            $(widgetFile "recommended-products")
        $(widgetFile "catalog")
    where
        fuser :: Maybe Text -> Handler (Maybe User) 
        fuser Nothing = return Nothing
        fuser (Just u) = acidQuery (UserByName u)


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


getProductRecR :: Int -> Handler RepHtml
getProductRecR threshold = undefined

---------------------------------------------------------------------------------------------------

productBuyAForm :: Product -> AForm Handler Product
productBuyAForm baseProduct = specificVariant 
    <$> areq (sizesField baseProduct) (i18nFieldSettings MsgProductSize) Nothing
    <*> areq (colorField baseProduct) (i18nFieldSettings MsgProductColor) Nothing
    where
        specificVariant size color = Product
            (productId baseProduct) 
            (productTitle baseProduct)
            (productSlot baseProduct)
            (productCategories baseProduct)
            (Set.singleton size)
            (Set.singleton color)
            (productRequirements baseProduct)
            (productAttributes baseProduct)
            (productDescription baseProduct)



productBuyForm :: Product -> Html -> MForm Handler (FormResult Product, Widget)
productBuyForm = renderTable . productBuyAForm

basicBuyForm :: ProductId -> Widget -> Enctype -> WidgetT Ecom IO ()
basicBuyForm pid widget enctype = toWidget $
    [whamlet|
    <form .pull-right method=post action=@{BuyProductR pid} enctype=#{enctype}>
        ^{widget}
        <button .btn-primary .btn-large .btn-buy .btn>
           <i class="icon-shopping-cart icon-white"></i> _{MsgBuyProduct} 
    |]



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

sizesField :: Product -> Field Handler ProductSize
sizesField = selectField . return . mkOptionList . (map mkSizeOption) . Set.toList . productSizes


colorField :: Product -> Field Handler ProductColor
colorField = colorRadioField . return . mkOptionList . (map mkColorOption) . Set.toList . productColors




mkColorOption :: ProductColor -> Option ProductColor
mkColorOption pcolor = Option 
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

colorRadioField :: Handler (OptionList ProductColor)
                -> Field Handler ProductColor
colorRadioField = selectFieldHelper
    -- outside
    (\theId _name _attrs inside -> [whamlet|
$newline never
<div ##{theId}>^{inside}
|])
    -- onOpt
    (\theId name isSel -> [whamlet|
$newline never
<label .radio for=#{theId}-none>
    <div>
        <input id=#{theId}-none type=radio name=#{name} value=none :isSel:checked>
        _{MsgSelectNone}
|])
    -- inside
    (\theId name attrs pcolor value isSel _ -> [whamlet| 
<label .radio for=#{theId}-#{value}>
    <div>
        <input id=#{theId}-#{value} type=radio name=#{name} value=#{value} :isSel:checked *{attrs}>
        \^{colorPreview (unProductColor pcolor) 25 25}
|])

