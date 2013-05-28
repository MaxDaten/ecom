{-# LANGUAGE CPP, UnicodeSyntax, TemplateHaskell, QuasiQuotes, DeriveDataTypeable
, GeneralizedNewtypeDeriving, TypeFamilies, OverloadedStrings, RecordWildCards, FlexibleInstances,
TypeSynonymInstances, DeriveGeneric, DefaultSignatures, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- mainly inspired by
-- https://github.com/HalfWayMan/meadowstalk.com/blob/a386797b7b1e470d841dbc9c2cc83b77de63fcab/src/Meadowstalk/Model.hs
-- Model.hs
module Ecom.Model where
----------------------------------------------------------------------------------------------------
--import           Prelude.Unicode
import           Prelude
import           Control.Applicative
import           Control.Monad              (mzero)
import           Control.Monad.Reader       (ask)
import           Control.Monad.State        (get, put)
----------------------------------------------------------------------------------------------------
import           Data.IxSet                 (Indexable (..), IxSet, (@=), Proxy (..), getOne, ixFun, ixSet)
import qualified Data.IxSet                 as IxSet
import           Data.Data
import           Data.Acid
import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Encode.Pretty   as Aeson ()
import           Data.String

import           Data.Text                  (Text, unpack, pack)
import           Data.Colour                ()
import           Data.Colour.SRGB

import           Data.SafeCopy              (SafeCopy (..), base, deriveSafeCopy)
import           Data.UUID
import           Data.UUID                  as UUID
import           Data.UUID.V4
--import Control.Monad.Trans     ( MonadIO(liftIO) )
import           GHC.Generics
----------------------------------------------------------------------------------------------------
import           Yesod.Core
----------------------------------------------------------------------------------------------------

data Product = Product
    { productId             :: ProductId
    , productTitle          :: ProductTitle
    , productCategory       :: ProductCategory
    , productSize           :: ProductSize
    , productColor          :: ProductColor
    , productDescription    :: ProductDescription
    }
    deriving (Eq, Ord, Data, Typeable, Show, Generic)

newtype ProductId = ProductId { unPostId :: UUID }
    deriving (Eq, Ord, Data, Typeable, Read, Show, Generic)

instance PathPiece ProductId where
    fromPathPiece x             = ProductId <$> fromPathPiece x
    toPathPiece (ProductId pid)  = toPathPiece pid

instance PathPiece UUID where
    fromPathPiece = UUID.fromString . unpack
    toPathPiece uuid = toPathPiece $ UUID.toString uuid

newtype ProductSize         = ProductSize        Int            deriving (Eq, Ord, Data, Typeable, SafeCopy, Show, Generic, ToJSON, FromJSON)
newtype ProductTitle        = ProductTitle       Text           deriving (Eq, Ord, Data, Typeable, SafeCopy, IsString, Show, Generic, ToJSON, FromJSON)
newtype ProductColor        = ProductColor      (RGB Double)    deriving (Eq, Ord, Data, Typeable, Show, Generic)
newtype ProductCategory     = ProductCategory    Text           deriving (Eq, Ord, Data, Typeable, SafeCopy, IsString, Show, Generic, ToJSON, FromJSON)
newtype ProductDescription  = ProductDescription Text           deriving (Eq, Ord, Data, Typeable, SafeCopy, IsString, Show, Generic, ToJSON, FromJSON)


deriving instance Data a => Data (RGB a)
deriving instance Typeable1 RGB
deriving instance Ord a => Ord (RGB a)


-- TH magic
deriveSafeCopy 0 'base ''Product
deriveSafeCopy 0 'base ''ProductColor
deriveSafeCopy 0 'base ''RGB
deriveSafeCopy 0 'base ''ProductId
deriveSafeCopy 0 'base ''UUID

instance Indexable Product where
    empty = ixSet
        [ ixFun $ \p -> [ productId          p ]
        , ixFun $ \p -> [ productTitle       p ]
        , ixFun $ \p -> [ productCategory    p ]
        , ixFun $ \p -> [ productSize        p ]
        , ixFun $ \p -> [ productColor       p ]
        --, ixFun $ \p -> [ productDescription p ]
        ]

-- seems to neccessary, for Product FromJSON and ToJSON are not derivable
instance FromJSON Product
instance ToJSON Product



instance FromJSON ProductId where
    parseJSON (Aeson.String v) = do
        let mUuid = UUID.fromString . unpack $ v
        case mUuid of
            (Just uuid) -> return $ ProductId uuid
            _ -> mzero
    parseJSON _ = mzero

instance ToJSON ProductId where
    toJSON (ProductId uuid) = Aeson.String . pack . toString $ uuid



-- custom json: we will write/read hex codes
instance FromJSON ProductColor where
    parseJSON (Aeson.String v) = return $ ProductColor . toSRGB . sRGB24read . unpack $ v
    parseJSON _ = mzero

instance ToJSON ProductColor where
    toJSON (ProductColor (RGB r g b)) = Aeson.String . pack . sRGB24show $ sRGB r g b


mkProduct :: ProductId -> Product
mkProduct pid =
    Product { productId             = pid
            , productTitle          = ""
            , productCategory       = ""
            , productColor          = ProductColor $ RGB 0 0 0
            , productSize           = ProductSize 0 -- TODO: maybe we will find some kind of autoboxing, if desired?
            , productDescription    = ""
            }

----------------------------------------------------------------------------------------------------

data EcomState = EcomState
    { catalog       :: IxSet Product
    }
    deriving (Data, Typeable)

deriveSafeCopy 0 'base ''EcomState


initialEcomState :: EcomState
initialEcomState = EcomState { catalog = IxSet.empty
                             }

----------------------------------------------------------------------------------------------------

-- acid-state query functions

fetchState :: Query EcomState EcomState
fetchState = ask

putState :: EcomState -> Update EcomState ()
putState = put


insertProduct :: Product -> Update EcomState Product
insertProduct p = do
    ecom@EcomState{..} <- get
    --nextProductId <- liftIO nextRandom
    --let p = mkProduct $ ProductId nextProductId

    put $ ecom { catalog = IxSet.insert p catalog }
    return p


updateProduct :: Product -> Update EcomState ()
updateProduct p = do
    ecom@EcomState{..} <- get
    put $ ecom { catalog = IxSet.updateIx (productId p) p catalog }


allProducts :: Query EcomState [Product]
allProducts = do
    EcomState{..} <- ask
    return $ IxSet.toDescList (Proxy :: Proxy ProductId) catalog


productById :: ProductId -> Query EcomState (Maybe Product)
productById pid = do
    EcomState{..} <- ask
    return $ getOne $ catalog @= pid


makeAcidic ''EcomState [ 'fetchState, 'putState
                       , 'insertProduct
                       , 'updateProduct
                       , 'allProducts
                       , 'productById
                       ]

----------------------------------------------------------------------------------------------------

-- in place testing, will be removed later on :P

{-
-- acid-state test
main :: IO ()
main = do
    print "Hellow State"
    state <- openLocalState initialEcomState
    --state <- openMemoryState initialEcomState -- import           Data.Acid.Memory
    allP <- query state AllProducts
    print $ "currently all products: " ++ show allP

    uuid <- nextRandom
    let prod = mkProduct (ProductId uuid)
    newP@Product{..} <- update state (InsertProduct prod)
    --update state (UpdateProduct newP {productTitle = "The Book about Worms", productCategory = "Book", productDescription = "a book about worms"})

    allP <- query state AllProducts
    print $ "now all products: " ++ show allP

    closeAcidState state
--}

{- json test
main :: IO ()
main = do
    print "parse json"

    --print bs
    mP <- BS.readFile "samples/product.json" >>= return . Aeson.decode
    case mP of
        Nothing -> print $ "error"
        Just p  -> do
                    print $ "Product: " ++ show (p :: Product)
                    BS.writeFile "samples/out.json" (Aeson.encodePretty p)
--}
