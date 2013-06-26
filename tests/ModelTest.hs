{-# LANGUAGE OverloadedStrings #-}
module ModelTest
    ( modelSpecs
    ) where

import              TestImport              ()
import              Test.Hspec

import              Ecom.Model
import              Data.Aeson              as Aeson
import              Data.UUID               (nil)
import              Data.Colour.SRGB        (RGB(..))
import qualified    Data.Set                as Set
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
        let defaultProduct = mkProduct (ProductId nil)
        let testProduct = Product 
                { productId             = ProductId nil
                , productTitle          = "Leichte Robe"
                , productSlot           = Head
                , productCategories     = Set.singleton (ProductCategory "Leichte Rüstung")
                , productColors         = Set.fromList [(ProductColor $ RGB 1 0 0), (ProductColor $ RGB 0 0 1)]
                , productSizes          = Set.fromList $ map ProductSize [12, 14, 15, 16]
                , productRequirements   = Attributes { str = Strength 0, int = Intelligence 0, dex = Dexterity 0, sta = Stamina 0 }
                , productAttributes     = Attributes { str = Strength 0, int = Intelligence 5, dex = Dexterity 0, sta = Stamina 5 }
                , productDescription    = "Eine schicke Robe für den modebewussten Magier"
                }

        -- just for introspection, i don't know how to print here directly
        -- this test is not possible, because of the not defined order of json fields
        --it "can encode a product to json" $ do
        --    Aeson.encode defaultProduct `shouldBe` ""

        it "can encode product to json and decode to json" $ do
            Aeson.decode (Aeson.encode defaultProduct) `shouldBe` (Just defaultProduct)

        it "can decode from sample \".product.json\" json" $ do
            mP <- BS.readFile (getFixure "product.json") >>= return . Aeson.decode
            mP `shouldBe` (Just testProduct)

    describe "user <-> json" $ do
        let user = mkUser "TestUser"

        it "can encode a user to json and decode it to json" $ do
            Aeson.decode (Aeson.encode user) `shouldBe` (Just user)

        it "can decode from fixure user.json" $ do
            mU <- BS.readFile (getFixure "user.json") >>= return . Aeson.decode
            mU `shouldBe` (Just user)
