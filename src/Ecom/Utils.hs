{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Ecom.Utils where

import           Data.Colour
import qualified Data.Text as T
import           Data.UUID
import           Data.Colour.SRGB (sRGB24show)
import 			 Data.Maybe (listToMaybe)
import 			 Control.Monad (unless)

import Ecom.Import

placeholditWidget :: Int -> Int -> WidgetT site IO ()
placeholditWidget width height = let (w, h) = (show width, show height) in
    toWidget [hamlet| <img .placeholdit .img-polaroid src=http://placehold.it/#{w}x#{h} style="width:#{w}; height:#{h};"> |]


colorPreview :: (RealFrac b, Floating b) => Colour b -> Int -> Int -> WidgetT site IO ()
colorPreview color width height = let (w, h) = (show width, show height) in do
    toWidget [hamlet| <span .color-preview style="background-color: #{colorHex color}; border-color: #{colorHex $ darken 0.7 color}"> |]
    toWidget [lucius| .color-preview { width: #{w}px; height: #{h}px; } |]



shortenUUID :: Int -> UUID -> String
shortenUUID n uuid = let s = toString uuid in (take n s) ++ "..." ++ (take n $ reverse s)

shortenDescription :: Int -> Text -> Text
shortenDescription n = (flip T.append $ "...") . T.strip . (T.take n)


colorHex :: (RealFrac b, Floating b) => Colour b -> String
colorHex = sRGB24show


slotMsg :: ProductSlot -> EcomMessage
slotMsg Head     = MsgSlotHead
slotMsg Torso    = MsgSlotTorso
slotMsg Legs     = MsgSlotLegs
slotMsg Feet     = MsgSlotFeet
slotMsg Hands    = MsgSlotHands
slotMsg LWeapon  = MsgSlotLWeapon
slotMsg RWeapon  = MsgSlotRWeapon
slotMsg LRing    = MsgSlotLRing
slotMsg RRing    = MsgSlotRRing
slotMsg Necklace = MsgSlotNecklace
slotMsg Artifact = MsgSlotArtifact
slotMsg Misc     = MsgSlotMisc
--slotMsg _        = undefined -- we want a warning


attribWidget :: (RenderMessage Ecom msg)
                 => msg -> Attributes -> WidgetT Ecom IO ()
attribWidget msg attributes@Attributes{..} = do
    -- mr <- getMessageRender
    toWidget [whamlet|
    <table>
        <caption>
            _{msg}
        <tr .attr-str>
            <td .attr-label>_{MsgAttribStrength}:
            <td .attr-value>#{unStr str}
        <tr .attr-int>
            <td .attr-label>_{MsgAttribIntelligence}:
            <td .attr-value>#{unInt int}
        <tr .attr-dex>
            <td .attr-label>_{MsgAttribDexterity}:
            <td .attr-value>#{unDex dex}
        <tr .attr-sta>
            <td .attr-label>_{MsgAttribStamina}:
            <td .attr-value>#{unSta sta}
    |]

-- copied from :/
-- http://hackage.haskell.org/packages/archive/yesod-form/1.3.0/doc/html/src/Yesod-Form-Fields.html#selectFieldHelper
-- modified by me to gain access to the raw value
selectFieldHelper
        :: (Eq a, RenderMessage site FormMessage)
        => (Text -> Text -> [(Text, Text)] -> WidgetT site IO () -> WidgetT site IO ())
        -> (Text -> Text -> Bool -> WidgetT site IO ())
        -> (Text -> Text -> [(Text, Text)] -> a -> Text -> Bool -> Text -> WidgetT site IO ())
        -> HandlerT site IO (OptionList a)
        -> Field (HandlerT site IO) a
selectFieldHelper outside onOpt inside opts' = Field
    { fieldParse = \x _ -> do
        opts <- opts'
        return $ selectParser opts x
    , fieldView = \theId name attrs val isReq -> do
        opts <- fmap olOptions $ handlerToWidget opts'
        outside theId name attrs $ do
            unless isReq $ onOpt theId name $ not $ render opts val `elem` map optionExternalValue opts
            flip mapM_ opts $ \opt -> inside
                theId
                name
                attrs
                (optionInternalValue opt)
                (optionExternalValue opt)
                ((render opts val) == optionExternalValue opt)
                (optionDisplay opt)
    , fieldEnctype = UrlEncoded
    }
  where
    render _ (Left _) = ""
    render opts (Right a) = maybe "" optionExternalValue $ listToMaybe $ filter ((== a) . optionInternalValue) opts
    selectParser _ [] = Right Nothing
    selectParser opts (s:_) = case s of
            "" -> Right Nothing
            "none" -> Right Nothing
            x -> case olReadExternal opts x of
                    Nothing -> Left $ SomeMessage $ MsgInvalidEntry x
                    Just y -> Right $ Just y

i18nFieldSettings :: (RenderMessage Ecom msg) => msg -> FieldSettings Ecom
i18nFieldSettings msg = FieldSettings (SomeMessage msg) Nothing Nothing Nothing []