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
----------------------------------------------------------------------------------------------------
import 				Ecom.Model
----------------------------------------------------------------------------------------------------

data SMModes 		= Import { inDir :: String
							 , purge :: Bool 
							 } 
			 		| Query  { all :: Bool }
			 		| Purge
	deriving (Show, Data, Typeable)

----------------------------------------------------------------------------------------------------
defDir = "samples"
queryDef  = Query  	{ all = True &= help "query everything in current state" }
importDef = Import 	{ inDir = defDir
							&= opt defDir 
							&= typDir 
							&= help ("input directory for samples [default: \"" ++ defDir ++ "\"]")
			    	, purge = def &= help "delete current state before import"
			 	    }
purgeDef  = Purge

argDef = modes $ [ queryDef  	&= help "query state" &= auto
				 , importDef 	&= help "import json samples in to state"
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

runStateManager :: SMModes -> AcidState EcomState -> IO ()
runStateManager Import{..} state = do
	when purge $ update state (PutState initialEcomState)

	sampleFiles <- getDirectoryContents inDir >>= return . (appendFolder inDir) . filterSamples

	print "read from: "
	mapM_ print sampleFiles

	products <- mapM ((liftM decodeSamples) . BS.readFile) sampleFiles

	print "parsed products:"
	mapM_ print products

	groupUpdates state $ map InsertProduct (catMaybes products)
	stateProducts <- query state AllProducts

	print "products in state"
	mapM_ print stateProducts
	
	where 
		filterSamples :: [FilePath] -> [FilePath]
		filterSamples = filter (\s -> isSample s && (not . isHidden) s)
		
		appendFolder :: FilePath -> [FilePath] -> [FilePath]
		appendFolder sampleFolder = map (combine sampleFolder)
		
		isSample = \s -> ".json" == takeExtension s
		isHidden = isPrefixOf "."
		
		decodeSamples :: BS.ByteString -> Maybe Product
		decodeSamples = Aeson.decode


runStateManager arg@Query{..} state = do
	stateProducts <- query state AllProducts
	print "all products:"
	mapM_ print stateProducts

runStateManager arg@Purge{..} state = update state (PutState initialEcomState)
