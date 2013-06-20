{-# LANGUAGE OverloadedStrings #-}
module ModelTest
    ( modelSpecs
    ) where

import              TestImport              ()
import              Test.Hspec

import              Ecom.Model
import              Data.Aeson              as Aeson
import              Data.UUID               (nil)
import qualified    Data.ByteString.Lazy    as BS
import              Control.Monad.Trans     ( MonadIO(liftIO) )

import              System.FilePath


fixureDir :: FilePath
fixureDir = combine "tests" "fixures"

getFixure :: FilePath -> FilePath
getFixure = combine fixureDir

modelSpecs :: Spec
modelSpecs = do
    describe "product <-> json" $ do
        let prod = mkProduct (ProductId nil)

        -- just for introspection, i don't know how to print here directly
        -- this test is not possible, because of the not defined order of json fields
        --it "can encode a product to json" $ do
        --    Aeson.encode prod `shouldBe` ""

        it "can encode product to json and decode to json" $ do
            Aeson.decode (Aeson.encode prod) `shouldBe` (Just prod)

        it "can decode from sample \".product.json\" json" $ do
            mP <- BS.readFile (getFixure "product.json") >>= return . Aeson.decode
            mP `shouldBe` (Just prod)

    describe "user <-> json" $ do
        let user = mkUser "TestUser"

        it "can encode a user to json and decode it to json" $ do
            Aeson.decode (Aeson.encode user) `shouldBe` (Just user)

        it "can decode from fixure user.json" $ do
            mU <- BS.readFile (getFixure "user.json") >>= return . Aeson.decode
            mU `shouldBe` (Just user)
