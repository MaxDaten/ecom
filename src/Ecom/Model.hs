{-# LANGUAGE CPP, UnicodeSyntax, TemplateHaskell, QuasiQuotes, DeriveDataTypeable
, GeneralizedNewtypeDeriving, TypeFamilies, OverloadedStrings, RecordWildCards, FlexibleInstances,
TypeSynonymInstances, DeriveGeneric, DefaultSignatures #-}
-- mainly inspired by
-- https://github.com/HalfWayMan/meadowstalk.com/blob/a386797b7b1e470d841dbc9c2cc83b77de63fcab/src/Meadowstalk/Model.hs
-- Model.hs
 module Ecom.Model (
    ) where
----------------------------------------------------------------------------------------------------
import           Prelude.Unicode
import           Control.Applicative
import           Control.Arrow
import           Control.Monad              (mzero)
import           Control.Monad.Reader       (ask)
import           Control.Monad.State        (get, put)
----------------------------------------------------------------------------------------------------
import           Data.IxSet                 (Indexable (..), IxSet, (@=), (@<), (@>), Proxy (..), getOne, ixFun, ixSet)
import qualified Data.IxSet                 as IxSet
import           Data.Data
import           Data.Acid
--import           Data.Acid.Memory
import           Data.Aeson                 ((.=), (.:))
import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Encode.Pretty   as Aeson
import           Data.String
import           Data.ByteString.Lazy       as BS
import           Data.Text                  (Text)
import           Data.SafeCopy              (SafeCopy, base, deriveSafeCopy)
import           GHC.Generics
----------------------------------------------------------------------------------------------------
import           Yesod.Core
----------------------------------------------------------------------------------------------------

data Product = Product
    { productId             :: ProductId
    , productTitle          :: ProductTitle
    , productCategory       :: ProductCategory
    , productDescription    :: ProductDescription
    }
    deriving (Eq, Ord, Data, Typeable, Show, Generic)

newtype ProductId = ProductId { unPostId :: Integer }
    deriving ( Eq, Ord, Data, Enum, Typeable, SafeCopy, Show, Generic, ToJSON, FromJSON)

newtype ProductTitle        = ProductTitle       Text   deriving (Eq, Ord, Data, Typeable, SafeCopy, IsString, Show, Generic, ToJSON, FromJSON)
newtype ProductCategory     = ProductCategory    Text   deriving (Eq, Ord, Data, Typeable, SafeCopy, IsString, Show, Generic, ToJSON, FromJSON)
newtype ProductDescription  = ProductDescription Text   deriving (Eq, Ord, Data, Typeable, SafeCopy, IsString, Show, Generic, ToJSON, FromJSON)

-- TH magic
deriveSafeCopy 0 'base ''Product

instance Indexable Product where
    empty = ixSet
        [ ixFun $ \p -> [ productId          p ]
        , ixFun $ \p -> [ productTitle       p ]
        , ixFun $ \p -> [ productCategory    p ]
        --, ixFun $ \p -> [ productDescription p ]
        ]

-- seems to neccessary, for Product FromJSON and ToJSON are not derivable
instance FromJSON Product where
instance ToJSON Product where

mkProduct :: ProductId -> Product
mkProduct id =
    Product { productId             = id
            , productTitle          = ""
            , productCategory       = ""
            , productDescription    = "xyz"
            }

----------------------------------------------------------------------------------------------------

data EcomState = EcomState
    { nextProductId :: ProductId
    , catalog       :: IxSet Product
    }
    deriving (Data, Typeable)

deriveSafeCopy 0 'base ''EcomState


initialEcomState :: EcomState
initialEcomState = EcomState { nextProductId = ProductId 1
                             , catalog = IxSet.empty
                             }

----------------------------------------------------------------------------------------------------

-- acid-state query functions

fetchState :: Query EcomState EcomState
fetchState = ask

putState :: EcomState -> Update EcomState ()
putState = put


newProduct :: Update EcomState Product
newProduct = do
    ecom@EcomState{..} <- get
    let product = mkProduct nextProductId
    put $ ecom { nextProductId  = succ nextProductId
               , catalog        = IxSet.insert product catalog
               }
    return product

updateProduct :: Product -> Update EcomState ()
updateProduct p = do
    ecom@EcomState{..} <- get
    put $ ecom { catalog = IxSet.updateIx (productId p) p catalog }

allProducts :: Query EcomState [Product]
allProducts = do
    ecom@EcomState{..} <- ask
    return $ IxSet.toDescList (Proxy :: Proxy ProductId) catalog


makeAcidic ''EcomState [ 'fetchState, 'putState
                       , 'newProduct
                       , 'updateProduct
                       , 'allProducts
                       ]

----------------------------------------------------------------------------------------------------

-- in place testing, will be removed later on :P

{- acid-state test
main :: IO ()
main = do
    print "Hellow State"
    --state <- openLocalState initialEcomState
    state <- openMemoryState initialEcomState -- import           Data.Acid.Memory
    allP <- query state AllProducts
    print $ "currently all products: " ++ show allP

    newP@Product{..} <- update state NewProduct
    update state (UpdateProduct newP {productTitle = "The Book about Worms", productCategory = "Book", productDescription = "a book about worms"})

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
