{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable
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
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Data
import           Data.Acid
import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Encode.Pretty   as Aeson ()
import           Data.String
import qualified Data.ByteString.Lazy       as BS
import           Data.Text                  (Text, unpack, pack)
import           Data.Colour                ()
import           Data.Colour.SRGB

import           Data.SafeCopy              (SafeCopy (..), base, deriveSafeCopy)
import           Data.UUID
import           Data.UUID                  as UUID
import           Data.UUID.V4
import           Data.UUID.V5
import           GHC.Generics
----------------------------------------------------------------------------------------------------
import           Yesod.Core
----------------------------------------------------------------------------------------------------

data Product = Product
    { productId             :: ProductId
    , productTitle          :: ProductTitle
    , productCategory       :: ProductCategory
    , productSizes          :: Set ProductSize
    , productColors         :: Set ProductColor
    , productDescription    :: ProductDescription
    }
    deriving (Eq, Ord, Data, Typeable, Show, Generic)

data Association = Association
    { assocCategory         :: ProductCategory
    , assocedCategories     :: Set ProductCategory
    }
    deriving (Eq, Ord, Data, Typeable, Show, Generic)


data User = User
    { username              :: Text
    , history               :: [Product]
    }
    deriving (Eq, Ord, Data, Typeable, Show, Generic)

newtype ProductId = ProductId { unProductId :: UUID }
    deriving (Eq, Ord, Data, Typeable, Read, Show, Generic)

instance PathPiece ProductId where
    fromPathPiece x             = ProductId <$> fromPathPiece x
    toPathPiece (ProductId pid)  = toPathPiece pid

instance PathPiece UUID where
    fromPathPiece = UUID.fromString . unpack
    toPathPiece uuid = toPathPiece $ UUID.toString uuid


newtype ProductColor        = ProductColor      (RGB Double)     deriving (Eq, Ord, Data, Typeable, Show, Generic)
newtype ProductSize         = ProductSize        Int             deriving (Eq, Ord, Data, Typeable, SafeCopy, Show, Generic, ToJSON, FromJSON)
newtype ProductTitle        = ProductTitle       Text            deriving (Eq, Ord, Data, Typeable, SafeCopy, IsString, Show, Generic, ToJSON, FromJSON)
newtype ProductCategory     = ProductCategory    Text            deriving (Eq, Ord, Data, Typeable, SafeCopy, IsString, Show, Generic, ToJSON, FromJSON)
newtype ProductDescription  = ProductDescription Text            deriving (Eq, Ord, Data, Typeable, SafeCopy, IsString, Show, Generic, ToJSON, FromJSON)

deriving instance Data a => Data (RGB a)
deriving instance Typeable1 RGB
deriving instance Ord a => Ord (RGB a)


-- TH magic
deriveSafeCopy 0 'base ''Product
deriveSafeCopy 0 'base ''ProductColor
deriveSafeCopy 0 'base ''RGB
deriveSafeCopy 0 'base ''ProductId
deriveSafeCopy 0 'base ''UUID
deriveSafeCopy 0 'base ''Association
deriveSafeCopy 0 'base ''User

----------------------------------------------------------------------------------------------------
instance Indexable Product where
    empty = ixSet
        [ ixFun $ \p -> [ productId          p ]
        , ixFun $ \p -> [ productTitle       p ]
        , ixFun $ \p -> [ productCategory    p ]
        , ixFun $ \p -> Set.toList $ productSizes  p
        , ixFun $ \p -> Set.toList $ productColors p
        ]

instance Indexable Association where
    empty = ixSet
        [ ixFun $ \a -> [ assocCategory a ]
        , ixFun $ \a -> Set.toList $ assocedCategories a
        ]

instance Indexable User where
    empty = ixSet
        [ ixFun $ \u -> [ username u ]
        , ixFun $ \u -> history u
        ]
----------------------------------------------------------------------------------------------------

instance FromJSON Product
instance ToJSON Product

instance FromJSON Association
instance ToJSON Association

instance FromJSON User
instance ToJSON User

deriving instance FromJSON ProductColor
deriving instance ToJSON ProductColor


instance FromJSON ProductId where
    parseJSON (Aeson.String v) = do
        let mUuid = UUID.fromString . unpack $ v
        case mUuid of
            (Just uuid) -> return $ ProductId uuid
            _ ->    mzero
    parseJSON _ = mzero

instance ToJSON ProductId where
    toJSON (ProductId uuid) = Aeson.String . pack . toString $ uuid

instance FromJSON (RGB Double) where
    parseJSON (Aeson.String s) = return $ toSRGB . sRGB24read . unpack $ s
    parseJSON _ = mzero

instance ToJSON (RGB Double) where
    toJSON (RGB r g b) = Aeson.String . pack . sRGB24show $ sRGB r g b

----------------------------------------------------------------------------------------------------

mkProduct :: ProductId -> Product
mkProduct pid =
    Product { productId             = pid
            , productTitle          = ""
            , productCategory       = ""
            , productColors         = Set.singleton (ProductColor $ RGB 0 0 0)
            , productSizes          = Set.singleton (ProductSize 0)
            , productDescription    = ""
            }

genProduct :: (MonadIO m) => m Product
genProduct = do
    uuid <- liftIO nextRandom
    return $ mkProduct (ProductId uuid)

genUUIDFromProduct :: Product -> UUID
genUUIDFromProduct p = generateNamed namespaceOID $ BS.unpack (Aeson.encode p)

----------------------------------------------------------------------------------------------------

data EcomState = EcomState { catalog :: IxSet Product
                           , assocs  :: IxSet Association
                           , users   :: IxSet User
                           }
    deriving (Data, Typeable)

deriveSafeCopy 0 'base ''EcomState


initialEcomState :: EcomState
initialEcomState = EcomState { catalog = IxSet.empty
                             , assocs  = IxSet.empty
                             , users   = IxSet.empty
                             }

----------------------------------------------------------------------------------------------------

-- acid-state query functions

fetchState :: Query EcomState EcomState
fetchState = ask

putState :: EcomState -> Update EcomState ()
putState = put


insertProduct :: Product -> Update EcomState ()
insertProduct p = do
    let prod = if (invalidId p) then p {productId = ProductId (genUUIDFromProduct p)} else p
    ecom@EcomState{..} <- get
    put $ ecom { catalog = IxSet.updateIx (productId prod) prod catalog }
    where
        invalidId = UUID.null . unProductId . productId


updateProduct :: Product -> Update EcomState ()
updateProduct = insertProduct


allProducts :: Query EcomState [Product]
allProducts = do
    EcomState{..} <- ask
    return $ IxSet.toDescList (Proxy :: Proxy ProductId) catalog


productById :: ProductId -> Query EcomState (Maybe Product)
productById pid = do
    EcomState{..} <- ask
    return $ getOne $ catalog @= pid


productByCategory :: ProductCategory -> Query EcomState [Product]
productByCategory = productByWhatever


productByWhatever :: (Typeable a) => a -> Query EcomState [Product]
productByWhatever x = do
    EcomState{..} <- ask
    return . IxSet.toList $ catalog @= x


insertAssoc :: Association -> Update EcomState ()
insertAssoc a = do
    ecom@EcomState{..} <- get
    put $ ecom { assocs = IxSet.updateIx (assocCategory a) a assocs }


combineAssoc :: Association -> Update EcomState ()
combineAssoc a = do
    EcomState{..} <- get
    let assoced    = map assocedCategories . IxSet.toList $ assocs @= (assocCategory a)
        newAssoced = foldr Set.union (assocedCategories a) assoced
    insertAssoc (Association { assocCategory = assocCategory a, assocedCategories = newAssoced })


allAssocs :: Query EcomState [Association]
allAssocs = do
    EcomState{..} <- ask
    return $ IxSet.toList assocs


assocByCategory :: ProductCategory -> Query EcomState [Association]
assocByCategory pCategory = do
    EcomState{..} <- ask
    return . IxSet.toList $ assocs @= pCategory


associatedProducts :: Product -> Query EcomState [Product]
associatedProducts p = do
    EcomState{..} <- ask
    let categories = concatMap (Set.toList . assocedCategories) $ IxSet.toList $ assocs @= (productCategory p)
        products   = concatMap (\pc -> IxSet.toList $ catalog @= pc) categories
    return products


similarProducts :: Product -> Query EcomState [Product]
similarProducts p = do
    EcomState{..} <- ask
    let matchingSize  = IxSet.toSet $ catalog @= (productSizes p)
        matchingColor = IxSet.toSet $ catalog @= (productColors p)
    return . Set.toList $ matchingSize `Set.union` matchingColor

----------------------------------------------------------------------------------------------------
-- annoying access -- maybe we will use some lenses?
getProductId :: Product -> UUID
getProductId Product{..} = pid
    where (ProductId pid) = productId

getProductTitle :: Product -> Text
getProductTitle Product{..} = t
    where (ProductTitle t) = productTitle

getProductDescription :: Product -> Text
getProductDescription Product{..} = d
    where (ProductDescription d) = productDescription

getProductColors :: Product -> [Colour Double]
getProductColors Product{..} = map unProductColor $ Set.toList productColors

getProductSizes :: Product -> [Int]
getProductSizes Product{..} = map unProductSize $ Set.toList productSizes

getProductCategory :: Product -> Text
getProductCategory Product{..} = cat
    where (ProductCategory cat) = productCategory

unProductColor :: ProductColor -> Colour Double
unProductColor (ProductColor (RGB r g b)) = sRGB r g b

unProductSize :: ProductSize -> Int
unProductSize (ProductSize s) = s

----------------------------------------------------------------------------------------------------


makeAcidic ''EcomState [ 'fetchState, 'putState
                       , 'insertProduct
                       , 'updateProduct
                       , 'allProducts
                       , 'productById
                       , 'insertAssoc
                       , 'combineAssoc
                       , 'allAssocs
                       , 'assocByCategory
                       , 'associatedProducts
                       , 'similarProducts
                       ]

----------------------------------------------------------------------------------------------------

