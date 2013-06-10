{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}

module StateManager where

import				Prelude	hiding (all)
import				Control.Monad
import 				Control.Exception 						(bracket)
import 				System.Directory
import 				System.FilePath
import 				System.Console.CmdArgs.Implicit
import				Data.List								(isSuffixOf, isPrefixOf)
import				Data.Maybe 								(catMaybes)
----------------------------------------------------------------------------------------------------
import 				Data.Acid
import 				Data.Acid.Advanced
import qualified 	Data.Aeson                 as Aeson
import qualified	Data.ByteString.Lazy	   as BS
import qualified    Data.UUID                  as UUID
----------------------------------------------------------------------------------------------------
import 				Ecom.Model
----------------------------------------------------------------------------------------------------

data SMModes 		= Import { inDir :: String
							 , purge :: Bool
							 }
                    | Export { outDir :: String
                             }
			 		| Query  { all :: Bool }
			 		| Purge
	deriving (Show, Data, Typeable)

----------------------------------------------------------------------------------------------------
defDir = "samples"
products = "products"
asscos = "assocs"
queryDef  = Query  { all = True &= help "query everything in current state" }
importDef = Import { inDir = defDir
                  &= opt defDir
                  &= typDir
                  &= help ("input directory for samples [default: \"" ++ defDir ++ "\"]")
                   , purge = def &= help "delete current state before import"
                   }
purgeDef  = Purge
exportDef = Export  { outDir = defDir
                             &= opt defDir
                             &= typDir
                             &= help ("output directory for exported state [default: \"" ++ defDir ++ "\"]")
                    }

argDef = modes $ [ queryDef  	&= help "query state" &= auto
                 , importDef    &= help "import json samples in to state"
				 , exportDef 	&= help "export state to json"
				 , purgeDef		&= help "empty current state"
				 ] &= summary "state-manager v0.1"
				   &= program "state-manager"

----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
	args <- cmdArgs argDef
	print args
	bracket
		(openLocalState initialEcomState)
		(closeAcidState)
		(runStateManager args)

----------------------------------------------------------------------------------------------------
-- | Import
runStateManager :: SMModes -> AcidState EcomState -> IO ()
runStateManager Import{..} state = do
  when purge $ update state (PutState initialEcomState)
  
  productFiles <- getDirectoryContents pDir >>= return . (appendFolder pDir) . filterSamples
  assocFiles   <- getDirectoryContents aDir >>= return . (appendFolder aDir) . filterSamples
  
  print "read from: "
  mapM_ print productFiles
  
  products <- mapM ((liftM decodeProducts) . BS.readFile) productFiles
  
  print "parsed products:"
  mapM_ print products
  
  print "read from: "
  mapM_ print assocFiles
  
  assocs <- mapM ((liftM decodeAssocs) . BS.readFile) assocFiles
  
  print "parsed assocs:"
  mapM_ print assocs
  
  groupUpdates state $ map InsertProduct (catMaybes products)
  groupUpdates state $ map CombineAssoc  (catMaybes assocs)
  
  print "products in state"
  stateProducts <- query state AllProducts
  mapM_ print stateProducts
  
  print "assocs in state"
  stateAssocs <- query state AllAssocs
  mapM_ print stateAssocs
  
  where
    filterSamples :: [FilePath] -> [FilePath]
    filterSamples = filter (\s -> isSample s && (not . isHidden) s)
    
    appendFolder :: FilePath -> [FilePath] -> [FilePath]
    appendFolder sampleFolder = map (combine sampleFolder)
    
    isSample = \s -> ".json" == takeExtension s
    isHidden = isPrefixOf "."
    
    decodeProducts :: BS.ByteString -> Maybe Product
    decodeProducts = Aeson.decode
    
    decodeAssocs   :: BS.ByteString -> Maybe Association
    decodeAssocs   = Aeson.decode
    
    pDir = inDir ++ "/" ++ products
    aDir = inDir ++ "/" ++ asscos

----------------------------------------------------------------------------------------------------
-- | Import
runStateManager arg@Query{..} state = do
	stateProducts <- query state AllProducts
	print "all products:"
	mapM_ print stateProducts
----------------------------------------------------------------------------------------------------
-- | Purge
runStateManager arg@Purge{..} state = update state (PutState initialEcomState)
----------------------------------------------------------------------------------------------------
-- | Export
runStateManager arg@Export{..} state = do
    stateProducts <- query state AllProducts

    print "export following entries:"
    mapM_ print stateProducts



    createDirectoryIfMissing True outDir

    let jsonProducts = zip stateProducts $ map Aeson.encode stateProducts
    mapM_ (writeEncoded outDir) jsonProducts

    where
        writeEncoded :: FilePath -> (Product, BS.ByteString) -> IO ()
        writeEncoded outDir (prod, jsonString) = BS.writeFile (combine outDir (getProductFilename prod)) jsonString
        getProductFilename prod = (UUID.toString . unProductId . productId $ prod) <.> "json"


