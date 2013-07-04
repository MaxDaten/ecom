{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards, TypeFamilies #-}

module StateManager where

import              Prelude hiding                  (all)
import              Control.Monad
import              Control.Exception               (bracket)
import              System.Directory
import              System.FilePath
import              System.Console.CmdArgs.Implicit
import              Data.List                       (isSuffixOf, isPrefixOf)
import              Data.Maybe                      (catMaybes)
import qualified    Data.Text                       as T
----------------------------------------------------------------------------------------------------
import              Data.Acid
import              Data.Acid.Advanced
import qualified    Data.Aeson                      as Aeson
import              Data.Aeson                      (FromJSON, ToJSON)
import qualified    Data.ByteString.Lazy            as BS
import qualified    Data.UUID                       as UUID
----------------------------------------------------------------------------------------------------
import              Ecom.Model
----------------------------------------------------------------------------------------------------

data SMModes    = Import { inDir          :: String
                         , purge          :: Bool
                         , importUsers    :: Bool
                         , importProducts :: Bool
                         , importAssocs   :: Bool
                         }
                | Export { outDir            :: String
                         , exportUsers       :: Bool
                         , exportProducts    :: Bool
                         , exportAssocs      :: Bool
                         }
                | Query  { all               :: Bool
                         , queryProducts     :: Bool
                         , queryAssosiations :: Bool
                         , queryUsers        :: Bool
                         }
                | Purge
  deriving (Show, Data, Typeable)

----------------------------------------------------------------------------------------------------
-- | directory definitions
defDir      = "samples"
defExports  = "exports"
productDir  = "products"
asscoDir    = "assocs"
userDir     = "users"

-- | programm mode definitions
queryDef  = Query  { all = True                &= help "query everything in current state"
                   , queryProducts      = def  &= name "products"         &= help "show all products in state"
                   , queryAssosiations  = def  &= name "assosiations"     &= help "show all assosiations" 
                   , queryUsers         = def  &= name "users"            &= help "show all users" }
importDef = Import { inDir = defDir
                          &= opt defDir
                          &= typDir
                          &= help ("input directory for samples [default: \"" ++ defDir ++ "\"]")
                   , purge = def &= help "delete current state before import"
                   , importUsers    = def &= name "users"    &= help "import users"
                   , importProducts = def &= name "products" &= help "import products"
                   , importAssocs   = def &= name "assocs"   &= help "import assocs"
                   }
purgeDef  = Purge
exportDef = Export  { outDir = defExports
                            &= opt defExports
                            &= typDir
                            &= help ("output directory for exported state [default: \"" ++ defExports ++ "\"]")
                    , exportUsers    = def &= name "users"      &= help "export users"
                    , exportProducts = def &= name "products"   &= help "export producs"
                    , exportAssocs   = def &= name "assocs"     &= help "export assocs"
                    }

argDef = modes $ [ queryDef   &= help "query state" &= auto
                 , importDef  &= help "import json samples in to state"
                 , exportDef  &= help "export state to json"
                 , purgeDef   &= help "empty current state"
                 ] &= summary "state-manager v0.2"
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

    let all = (importProducts || importAssocs || importUsers) `xor` True

    -- import with [State-Update] from [Directory] - [Maybe Query] to check state
    when (all || importProducts) $
        importWith InsertProduct (inDir </> productDir) $ Just AllProducts
    
    when (all || importAssocs) $
        importWith CombineAssoc (inDir </> asscoDir) $ Just AllAssocs

    when (all || importUsers) $ 
        importWith InsertUser (inDir </> userDir) $ Just AllUsers

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
-- | Query
runStateManager arg@Query{..} state = do
    let all' = all `xor` (queryProducts || queryAssosiations || queryUsers)

    when (all' || queryProducts) $ do
        stateProducts <- query state AllProducts
        print "all products:"
        mapM_ print stateProducts

    when (all' || queryAssosiations) $ do
        stateAssocs <- query state AllAssocs
        print "all assosiations:"
        mapM_ print stateAssocs

    when (all' || queryUsers) $ do
        stateUsers <- query state AllUsers
        print "all users:"
        mapM_ print stateUsers

----------------------------------------------------------------------------------------------------
-- | Purge
runStateManager arg@Purge{..} state = update state (PutState initialEcomState)

----------------------------------------------------------------------------------------------------
-- | Export
runStateManager arg@Export{..} state = do
    let all = (exportProducts || exportAssocs || exportUsers) `xor` True

    when (all || exportProducts) $
        exportWith AllProducts (outDir </> productDir)

    when (all || exportAssocs) $
        exportWith AllAssocs (outDir </> asscoDir)

    when (all || exportUsers) $
        exportWith AllUsers (outDir </> userDir)

    where
        exportWith :: (QueryEvent queryEvent, MethodState queryEvent ~ EcomState, MethodResult queryEvent ~ [a]
                      , ToJSON a, Show a, Fileable a) 
                   => queryEvent -> FilePath -> IO ()
        exportWith stQuery outDir = do
            states <- query state stQuery

            print "export following entries:"
            mapM_ print states

            createDirectoryIfMissing True outDir

            let jsons = zip states $ map Aeson.encode states
            mapM_ (writeEncoded outDir) jsons            

        writeEncoded :: (ToJSON a, Fileable a) => FilePath -> (a, BS.ByteString) -> IO ()
        writeEncoded outDir (st, jsonString) = BS.writeFile (outDir </> getFilename st <.> "json") jsonString

xor :: Bool -> Bool -> Bool
xor True p  = not p
xor False p = p


class Fileable a where
    getFilename :: a -> FilePath


instance Fileable Association where
    getFilename = T.unpack . unProductCategory . assocCategory


instance Fileable Product where
    getFilename = UUID.toString . unProductId . productId


instance Fileable User where
    getFilename = T.unpack . username
