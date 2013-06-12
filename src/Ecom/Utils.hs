module Ecom.Utils where

import Ecom.Import

placeholditWidget :: Int -> Int -> WidgetT site IO ()
placeholditWidget width height = do
	let (w, h) = (show width, show height)
	toWidget [hamlet| <img .placeholdit .img-polaroid src=http://placehold.it/#{w}x#{h} style="width:#{w}; height:#{h};"> |]