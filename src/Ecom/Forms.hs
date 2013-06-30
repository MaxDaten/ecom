{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-name-shadowing -fno-warn-hi-shadowing #-}
module Ecom.Forms where


import 				Ecom.Import
import 				Ecom.Utils

import             	Data.Set                 (Set)
import qualified 	Data.Set                  as Set
import             	Data.List.Split
import              Control.Arrow            ((&&&))
import 				Data.Text                (pack, unpack)
import             	Data.UUID                (nil)
import 				Data.Colour.SRGB         (sRGB24show, toSRGB, sRGB24read)

---------------------------------------------------------------------------------------------------

userAForm :: AForm Handler User
userAForm = User 
	<$> areq textField "Name" Nothing
	<*> pure [] -- empty history
	<*> attributesAForm


userForm :: Html -> MForm Handler (FormResult User, Widget)
userForm = renderDivs userAForm

basicUserForm :: Widget -> Enctype -> WidgetT Ecom IO ()
basicUserForm widget enctype = toWidget $ 
    [whamlet|
    <form method=post action=@{AdminAllUsersR} enctype=#{enctype}>
        ^{widget}
        <button .btn-primary .btn>_{MsgSubmit}
   |]

---------------------------------------------------------------------------------------------------

productAForm :: AForm Handler Product
productAForm = Product 
    (ProductId nil) 
    <$> (ProductTitle            <$> areq textField (i18nFieldSettings MsgProductTitle) Nothing)
    <*> areq (selectFieldList slotOptions) (i18nFieldSettings MsgProductSlot) (Just (maxBound :: ProductSlot))
    <*> (fromCVS ProductCategory <$> areq textField (i18nFieldSettings MsgProductCategories) Nothing)
    <*> (fromCVS mkPSizes        <$> areq textField (i18nFieldSettings MsgProductSizes) Nothing)
    <*> (fromCVS mkPColors       <$> areq textField (i18nFieldSettings MsgProductColors) Nothing)
    <*> attributesAForm
    <*> attributesAForm
    <*> (mkPDescription          <$> areq textareaField (i18nFieldSettings MsgProductDescription) Nothing)

    where
        fromCVS :: (Ord a) => (Text -> a) -> Text -> Set a
        fromCVS ctor    = Set.fromList . map (ctor . pack) . splitOn "," . unpack
        mkPSizes        = ProductSize . read . unpack
        mkPColors       = ProductColor . toSRGB . sRGB24read . unpack
        mkPDescription  = ProductDescription . unTextarea
        slotOptions    :: [(EcomMessage, ProductSlot)]
        slotOptions     = map (slotMsg &&& id) [minBound..maxBound]

productMForm :: Html -> MForm Handler (FormResult Product, Widget)
productMForm extra = do  
    (titleRes, titleView)    <- mreq textField (i18nFieldSettings MsgProductTitle) Nothing
    (slotRes,  slotView)     <- mreq (selectFieldList slotOptions) (i18nFieldSettings MsgProductSlot) (Just (maxBound :: ProductSlot))
    (catRes,   catView)      <- mreq textField (i18nFieldSettings MsgProductCategories) Nothing
    (sizeRes,  sizeView)     <- mreq textField (i18nFieldSettings MsgProductSizes) Nothing
    (colRes,   colView)      <- mreq textField (i18nFieldSettings MsgProductColors) Nothing
    (reqRes,   reqViews')    <- aFormToForm $ attributesAForm
    (boniRes,  boniViews')   <- aFormToForm $ attributesAForm
    (descRes,  descView)     <- mreq textareaField (i18nFieldSettings MsgProductDescription) Nothing
    let reqViews    = reqViews' []
        boniViews   = boniViews' []
        productRes  = Product (ProductId nil)
                    <$> (ProductTitle <$> titleRes)
                    <*> slotRes
                    <*> (fromCVS ProductCategory <$> catRes)
                    <*> (fromCVS mkPSizes <$> sizeRes)
                    <*> (fromCVS mkPColors <$> colRes)
                    <*> reqRes
                    <*> boniRes
                    <*> (mkPDescription <$> descRes)
        baseViews = [titleView, slotView, catView, sizeView, colView, descView]
    let widget = do
        baseFormId <- newIdent
        reqFormId  <- newIdent
        boniFormId <- newIdent
        toWidget [lucius|
        ##{baseFormId} {

        }
        ##{reqFormId}, ##{boniFormId} {
            border: 1px red solid;
            border-radius: 3px;
            background-color: lightgrey;

            input {
                width: 3.5em;
            }
        }
        ##{reqFormId} {
            border-color: red;
        }
        ##{boniFormId} {
            border-color: green;
        }
        |]
        toWidget [whamlet|
        #{extra}
        <div .row>
            <div ##{baseFormId} .span4>
                <table>
                    $forall v <- baseViews
                        <tr>
                            <td>
                                ^{fvLabel v}
                            <td>
                                ^{fvInput v}
            <div ##{reqFormId} .span2>
                <table>
                    <caption>_{MsgProductRequirements}
                    $forall reqView <- reqViews
                        <tr>
                            <td>
                                ^{fvLabel reqView}
                            <td>
                                ^{fvInput reqView}
            <div ##{boniFormId} .span2>
                <table>
                    <caption>_{MsgProductBoni}
                    $forall boniView <- boniViews
                        <tr>
                            <td>
                                ^{fvLabel boniView}
                            <td>
                                ^{fvInput boniView}
        |]
    return (productRes, widget)
    where
        fromCVS :: (Ord a) => (Text -> a) -> Text -> Set a
        fromCVS ctor    = Set.fromList . map (ctor . pack) . splitOn "," . unpack
        mkPSizes        = ProductSize . read . unpack
        mkPColors       = ProductColor . toSRGB . sRGB24read . unpack
        mkPDescription  = ProductDescription . unTextarea
        slotOptions    :: [(EcomMessage, ProductSlot)]
        slotOptions     = map (slotMsg &&& id) [minBound..maxBound]

productForm :: Html -> MForm Handler (FormResult Product, Widget)
productForm = productMForm

basicProductForm :: Widget -> Enctype -> WidgetT Ecom IO ()
basicProductForm widget enctype = toWidget $ 
    [whamlet|
    <form method=post action=@{AdminCreateProductR} enctype=#{enctype}>
        ^{widget}
        <button .btn-primary .btn>_{MsgSubmit}
   |]

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

---------------------------------------------------------------------------------------------------

attributesAFormWithDefault :: Attributes -> AForm Handler Attributes
attributesAFormWithDefault attrs = Attributes
    <$> (Strength     <$> areq intField (i18nFieldSettings MsgAttribStrength) (Just . unStr $ str attrs))
    <*> (Intelligence <$> areq intField (i18nFieldSettings MsgAttribIntelligence) (Just . unInt $ int attrs))
    <*> (Dexterity    <$> areq intField (i18nFieldSettings MsgAttribDexterity) (Just . unDex $ dex attrs))
    <*> (Stamina      <$> areq intField (i18nFieldSettings MsgAttribStamina) (Just . unSta $ sta attrs))

attributesAForm :: AForm Handler Attributes
attributesAForm = attributesAFormWithDefault mkAttributes


---------------------------------------------------------------------------------------------------
