module Ecom.Utils where

import Data.Colour
import Data.UUID
import Data.Colour.SRGB (sRGB24show)

import Ecom.Import

placeholditWidget :: Int -> Int -> WidgetT site IO ()
placeholditWidget width height = let (w, h) = (show width, show height) in
    toWidget [hamlet| <img .placeholdit .img-polaroid src=http://placehold.it/#{w}x#{h} style="width:#{w}; height:#{h};"> |]


colorPreview :: (RealFrac b, Floating b) => Colour b -> Int -> Int -> WidgetT site IO ()
colorPreview color width height = let (w, h) = (show width, show height) in do
    toWidget [hamlet| <span .color-preview style="background-color: #{colorHex color}; border-color: #{colorHex $ darken 0.7 color}"> |]
    toWidget [lucius| .color-preview{ width: #{w}px; height: #{h}px} |]



shortenUUID :: Int -> UUID -> String
shortenUUID n uuid = let s = toString uuid in (take n s) ++ "..." ++ (take n $ reverse s)


colorHex :: (RealFrac b, Floating b) => Colour b -> String
colorHex = sRGB24show