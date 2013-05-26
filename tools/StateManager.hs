{-# LANGUAGE CPP, OverloadedStrings #-}

module StateManager where

import				Prelude
import				Control.Monad
import 				System.Directory
import				Data.List			(isSuffixOf, isPrefixOf)
----------------------------------------------------------------------------------------------------
import 				Data.Acid
----------------------------------------------------------------------------------------------------
import 				Ecom.Model
----------------------------------------------------------------------------------------------------


main :: IO ()
main = initializeStateWithSamplesFrom "samples"


initializeStateWithSamplesFrom :: FilePath -> IO ()
initializeStateWithSamplesFrom sampleFolder = do
	state <- openLocalState initialEcomState
	sampleFiles <- getDirectoryContents sampleFolder >>= return . filterSamples
	print sampleFiles
	closeAcidState state
	where 
		filterSamples :: [FilePath] -> [FilePath]
		filterSamples = filter (\s -> isSample s && (not . isHidden) s)
		isSample = isSuffixOf ".json" 
		isHidden = isPrefixOf "."

