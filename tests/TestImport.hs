{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module Yesod.Test
    , module Foundation
    , Specs
    ) where

import Yesod.Test
import Ecom.Foundation as Foundation

type Specs = YesodSpec Ecom
