{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Ecom.Handler.Home where

import Ecom.Import
import Ecom.Utils
import Control.Monad (join, unless)
import Data.Maybe (maybeToList)
import Text.Julius (rawJS)

getHomeR :: Handler RepHtml
getHomeR = do
    redirect CatalogR

