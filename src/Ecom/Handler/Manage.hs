module Ecom.Handler.Manage where

import Data.Colour (darken)

import Ecom.Import
import Ecom.Utils


getManageUsersR :: Handler RepHtml
getManageUsersR = defaultLayout $ do
        setTitle "Manage Users"
        $(widgetFile "manage-users")


getManageProductsR :: Handler RepHtml
getManageProductsR = do
    allProducts <- acidQuery (AllProducts)
    defaultLayout $ do
        setTitle "Manage Products"
        $(widgetFile "manage-products")


getManageAssocsR :: Handler RepHtml
getManageAssocsR = defaultLayout $ do
    setTitle "Manage Assocs"
    $(widgetFile "manage-assocs")
