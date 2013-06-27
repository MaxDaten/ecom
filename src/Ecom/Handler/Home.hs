{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Ecom.Handler.Home where

import Ecom.Import

getHomeR :: Handler RepHtml
getHomeR = do
    redirect CatalogR

