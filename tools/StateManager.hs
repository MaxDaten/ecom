{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards, TypeFamilies #-}

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
import qualified    Data.Aeson                 as Aeson
import           	Data.Aeson                 (FromJSON)
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
			 		| Query  { all               :: Bool
                             , queryProducts     :: Bool
                             , queryAssosiations :: Bool
                             }
			 		| Purge
	deriving (Show, Data, Typeable)

----------------------------------------------------------------------------------------------------
-- | directory definitions
defDir      = "samples"
productDir  = \i -> combine i "products"
asscoDir    = \i -> combine i "assocs"

-- | programm mode definitions
queryDef  = Query  { all = True                &= help "query everything in current state"
                   , queryProducts      = def  &= name "products"         &= help "show all products in state"
                   , queryAssosiations  = def  &= name "assosiations"     &= help "show all assosiations" }
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

    -- import with [State-Update] from [Directory] - [Maybe Query] to check state
    importWith InsertProduct (productDir inDir) $ Just AllProducts
    importWith CombineAssoc (asscoDir inDir) $ Just AllAssocs

    where
        filterSamples :: [FilePath] -> [FilePath]
        filterSamples = filter (\s -> isSample s && (not . isHidden) s)

        appendFolder :: FilePath -> [FilePath] -> [FilePath]
        appendFolder sampleFolder = map (combine sampleFolder)

        isSample = \s -> ".json" == takeExtension s
        isHidden = isPrefixOf "."

        -- what a hell of types families and instances :) hell yea
        importWith :: ( UpdateEvent updateEvent, MethodState updateEvent ~ EcomState                                -- our updateEvent is an official UpdateEvent and works in our EcomState
                      , QueryEvent queryEvent, MethodState queryEvent ~ EcomState, MethodResult queryEvent ~ [a]    -- our queryEvent is an offical QueryEvent, works in our EcomState and has a Resul like a (see next line)
                      , FromJSON a, Show a)                                                                         -- our a is decodeable from JSON and just for fun showable
                   => (a -> updateEvent) -> FilePath -> Maybe queryEvent -> IO ()
        importWith stUpdate dir chkQuery = do
            sampleFiles <- getDirectoryContents dir >>= return . (appendFolder dir) . filterSamples

            samples <- mapM ((liftM Aeson.decode) . BS.readFile) sampleFiles

            print "parsed samples:"
            mapM_ print samples

            print "read from: "
            mapM_ print sampleFiles

            groupUpdates state $ map stUpdate (catMaybes samples)

            case chkQuery of
                Just qry -> do
                    print "samples in state"
                    stateSamples <- query state qry
                    mapM_ print stateSamples
                _ -> return ()

----------------------------------------------------------------------------------------------------
-- | Import
runStateManager arg@Query{..} state = do
    let all' = all `xor` (queryProducts || queryAssosiations)

    when (all' || queryProducts) $ do
        stateProducts <- query state AllProducts
        print "all products:"
        mapM_ print stateProducts

    when (all' || queryAssosiations) $ do
        stateAssocs <- query state AllAssocs
        print "all assosiations:"
        mapM_ print stateAssocs

    where
        xor :: Bool -> Bool -> Bool
        xor True p = not p
        xor False p = p

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


