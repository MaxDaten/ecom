{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

--import Ecom.Import
--import Yesod.Default.Config
--import Yesod.Test
import Test.Hspec (hspec, describe)
--import Ecom.Application (makeFoundation)

--import HomeTest
import ModelTest

main :: IO ()
main = do
    --conf <- Yesod.Default.Config.loadConfig $ (configSettings Testing)
    --            { csParseExtra = parseExtra
    --            }
    --foundation <- makeFoundation conf
    hspec $ do
        --yesodSpec foundation $ do
        --    homeSpecs


        describe "parse product" $ do
            modelSpecs
