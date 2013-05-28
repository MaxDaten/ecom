{-# LANGUAGE OverloadedStrings #-}
module ModelTest
    ( modelSpecs
    ) where

import TestImport ()
import Test.Hspec

import Ecom.Model
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy       as BS

modelSpecs :: Spec
modelSpecs =
	describe "product <-> json" $ do
        let prod   = mkProduct $ ProductId 42
        it "can encode product to json and decode to json" $ do
            Aeson.decode (Aeson.encode prod) `shouldBe` (Just prod)

        it "can decode from sample \".product.json\" json" $ do
            mP <- BS.readFile "samples/.product.json" >>= return . Aeson.decode
            mP `shouldBe` (Just prod)
