{-# LANGUAGE CPP, OverloadedStrings #-}

module StateManager where

import				Prelude
import				Control.Monad
import 				Control.Exception (bracket)
import 				System.Directory
import 				System.FilePath
import				Data.List			(isSuffixOf, isPrefixOf)
import				Data.Maybe 			(catMaybes)
----------------------------------------------------------------------------------------------------
import 				Data.Acid
import 				Data.Acid.Advanced
import qualified 	Data.Aeson                 as Aeson
import qualified	Data.ByteString.Lazy	   as BS
----------------------------------------------------------------------------------------------------
import 				Ecom.Model
----------------------------------------------------------------------------------------------------


main :: IO ()
main = bracket 
			(openLocalState initialEcomState)
			(closeAcidState) 
			(initializeStateWithSamplesFrom "samples")




initializeStateWithSamplesFrom :: FilePath -> AcidState EcomState -> IO ()
initializeStateWithSamplesFrom sampleFolder state = do
	update state (PutState initialEcomState)

	sampleFiles <- getDirectoryContents sampleFolder >>= return . filterSamples . (appendFolder sampleFolder)

	print sampleFiles
	products <- mapM ((liftM decodeSamples) . BS.readFile) sampleFiles
	mapM_ print products

	-- insert products to state
	groupUpdates state $ map InsertProduct (catMaybes products)

	stateProducts <- query state AllProducts
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

