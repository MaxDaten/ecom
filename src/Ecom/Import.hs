{-# LANGUAGE CPP #-}
module Ecom.Import
    ( module Import
    ) where

import      Prelude                         as Import hiding (head, init, last,
                                                 readFile, tail, writeFile)
import      Yesod                           as Import hiding (Route (..))

import      Control.Applicative             as Import (pure, (<$>), (<*>))
import      Data.Text                       as Import (Text)

import      Ecom.Foundation                 as Import
import      Ecom.Settings.Development       as Import
import      Ecom.Settings                   as Import
import      Ecom.Settings.StaticFiles       as Import
import      Ecom.Model                      as Import

#if __GLASGOW_HASKELL__ >= 704
import      Data.Monoid                     as Import
                                                 (Monoid (mappend, mempty, mconcat),
                                                 (<>))
#else
import      Data.Monoid                     as Import
                                                (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif
