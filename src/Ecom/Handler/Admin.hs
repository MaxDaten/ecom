{-# LANGUAGE TupleSections, OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Ecom.Handler.Admin where

import Ecom.Import
import Ecom.Utils
import Data.UUID (nil)
import           Data.Colour                ()
import           Data.Colour.SRGB

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split
import Data.Text (unpack, pack)


getAdminAllUsersR :: Handler RepHtml
getAdminAllUsersR = do
        allUsers <- acidQuery AllUsers
        defaultLayout $ do
            setTitle "Admin Users"
            $(widgetFile "admin-users")
            $(widgetFile "table")


postAdminAllUsersR :: Handler RepHtml
postAdminAllUsersR = do
    ((result, _), _) <- runFormPost userForm
    case result of
        FormSuccess user -> do
            mUser <- acidQuery (UserByName (username user))
            case mUser of
                Nothing -> do
                    acidUpdate (InsertUser user)
                    setInfoMessageI $ MsgUserCreated (username user)
                _ -> setErrorMessageI MsgUserAlreadyExisting
        _ -> setErrorMessageI MsgInvalidInput
    redirect AdminAllUsersR


postAdminClearHistoryR :: Text -> Handler RepHtml
postAdminClearHistoryR username = do
    mUser <- acidQuery (UserByName username)
    case mUser of
        Nothing -> setErrorMessageI MsgInvalidUser
        Just user -> do
            acidUpdate (ClearUserHistory user)
            setInfoMessageI MsgUserHistoryCleared
    redirect AdminAllUsersR
            

postAdminDeleteHistoryEntry :: Text -> Int -> Handler RepHtml
postAdminDeleteHistoryEntry username i = do
    mUser <- acidQuery (UserByName username)
    case mUser of
        Nothing -> do 
            setErrorMessageI MsgInvalidUser
            redirect AdminAllUsersR
        Just user -> do
            acidUpdate (DeleteHistoryEntry user i)
            setInfoMessageI MsgUserHistoryEntryDeleted
            redirect (AdminUserR username)


getAdminCreateUserR :: Handler RepHtml
getAdminCreateUserR = do
    (widget, enctype) <- generateFormPost userForm
    defaultLayout [whamlet|
    ^{basicUserForm widget enctype}
    <a .btn href=@{AdminAllUsersR}>« _{MsgBack}|]


getAdminCreateProductR :: Handler RepHtml
getAdminCreateProductR = do
    (widget, enctype) <- generateFormPost productForm
    defaultLayout [whamlet|
    ^{basicProductForm widget enctype}
    <a .btn href=@{AdminAllUsersR}>« _{MsgBack}|]

postAdminCreateProductR :: Handler RepHtml
postAdminCreateProductR = do
    ((result, _), _) <- runFormPost productForm
    case result of
        FormSuccess product -> do
            lift $ print (show product)
            acidUpdate (InsertProduct product)
            setInfoMessageI MsgProductAdded
        _ -> setErrorMessageI MsgInvalidInput
    redirect AdminAllProductsR


getAdminDeleteUserR :: Text -> Handler RepHtml
getAdminDeleteUserR name = do
    acidUpdate (DeleteUserByName name)
    redirect AdminAllUsersR


getAdminUserR :: Text -> Handler RepHtml
getAdminUserR = getUserR


getUserR :: Text -> Handler RepHtml
getUserR name = do
    mU <- acidQuery (UserByName name)
    case mU of
        Nothing -> notFound
        (Just user) -> defaultLayout $ do
            setTitle $ toHtml $ "User: " ++ show (username user)
            $(widgetFile "user")
    where idxList = zip ([0..])

---------------------------------------------------------------------------------------------------

userAForm :: AForm Handler User
userAForm = mkUser <$> areq textField "Name" Nothing


userForm :: Html -> MForm Handler (FormResult User, Widget)
userForm = renderDivs userAForm

basicUserForm :: Widget -> Enctype -> WidgetT Ecom IO ()
basicUserForm widget enctype = toWidget $ 
    [whamlet|
    <form method=post action=@{AdminAllUsersR} enctype=#{enctype}>
        ^{widget}
        <button .btn-primary .btn>_{MsgSubmit}
   |]

---------------------------------------------------------------------------------------------------

productAForm :: AForm Handler Product
productAForm = Product 
    (ProductId nil) 
    <$> (ProductTitle            <$> areq textField (i18nFieldSettings MsgProductTitle) Nothing)
    <*> (ProductSlot             <$> areq textField (i18nFieldSettings MsgProductSlot) Nothing) -- enum radio
    <*> (fromCVS ProductCategory <$> areq textField (i18nFieldSettings MsgProductCategories) Nothing)
    <*> (fromCVS (ProductSize . read . unpack) <$> areq textField (i18nFieldSettings MsgProductSizes) Nothing)
    <*> (fromCVS (ProductColor . toSRGB . sRGB24read . unpack) <$> areq textField (i18nFieldSettings MsgProductColors) Nothing)
    <*> attributesAForm
    <*> attributesAForm
    <*> (ProductDescription . unTextarea <$> areq textareaField (i18nFieldSettings MsgProductDescription) Nothing)

    where
        fromCVS :: (Ord a) => (Text -> a) -> Text -> Set a
        fromCVS ctor = Set.fromList . map (ctor . pack) . splitOn "," . unpack 

attributesAForm :: AForm Handler Attributes
attributesAForm = Attributes
    <$> (Strength     <$> areq intField (i18nFieldSettings MsgAttribStrength) (Just 0))
    <*> (Intelligence <$> areq intField (i18nFieldSettings MsgAttribIntelligence) (Just 0))
    <*> (Dexterity    <$> areq intField (i18nFieldSettings MsgAttribDexterity) (Just 0))
    <*> (Stamina      <$> areq intField (i18nFieldSettings MsgAttribStamina) (Just 0))

productForm :: Html -> MForm Handler (FormResult Product, Widget)
productForm = renderDivs productAForm

basicProductForm :: Widget -> Enctype -> WidgetT Ecom IO ()
basicProductForm widget enctype = toWidget $ 
    [whamlet|
    <form method=post action=@{AdminCreateProductR} enctype=#{enctype}>
        ^{widget}
        <button .btn-primary .btn>_{MsgSubmit}
   |]

---------------------------------------------------------------------------------------------------

getAdminAllProductsR :: Handler RepHtml
getAdminAllProductsR = do
    allProducts <- acidQuery (AllProducts)
    defaultLayout $ do
        setTitle "Admin Products"
        $(widgetFile "admin-products")
        $(widgetFile "table")


getAdminAssocsR :: Handler RepHtml
getAdminAssocsR = defaultLayout $ do
    setTitle "Admin Assocs"
    $(widgetFile "admin-assocs")
