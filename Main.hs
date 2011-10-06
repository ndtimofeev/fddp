module Main where

-- base
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.IO (IOMode(..), hFileSize, withBinaryFile)

-- directory
import System.Directory (doesFileExist, canonicalizePath)

-- enumerator
import Data.Enumerator (($$), run_)
import Data.Enumerator.Binary (enumFile, consume)

-- transformers
import Control.Monad.Trans.Error (ErrorT(..), Error(..), throwError)
import Control.Monad.Trans.Class (lift)

data FileData = FileData
    { maybeSize      :: Maybe Integer
    , maybeCanonical :: Maybe FilePath
    , pathToFile     :: FilePath
    } deriving (Show, Eq, Ord)

data Exception = BadKey String | BadFile String | Exception String deriving Show

data Equal = Equal | Unequal | Identical deriving Show

data CompareReport = CompareReport 
    { equal   :: Equal
    , newOrig :: Maybe FileData
    , newNext :: Maybe FileData
    } deriving Show

instance Error Exception where
    strMsg = Exception

main :: IO (Either Exception ())
main = runErrorT $ do
    _argv <- lift getArgs
    _argv <- mapM (\p -> checkFile p >> return p >>= buildFileData) _argv
    process _argv

checkFile :: FilePath -> ErrorT Exception IO ()
checkFile path = do
    check <- lift $ doesFileExist path
    unless check $ throwError $ BadFile path

buildFileData :: FilePath -> ErrorT Exception IO FileData
buildFileData path = return $ FileData Nothing Nothing path

process :: [FileData] -> ErrorT Exception IO ()
process files = case files of
    (x:xs) -> do
        lift $ putStrLn $ pathToFile x
        ufiles <- lift $ fileFilter x xs
        process ufiles
    []     -> return ()

fileFilter :: FileData -> [FileData] -> IO [FileData]
fileFilter file files = case files of
    (x:xs) -> do
        ret <- compareFile file x
        case ret of
            CompareReport Identical _ _    -> error "Кровь, кишки, слёзы"
            CompareReport Equal nfile _    -> fileFilter (fromMaybe file nfile) xs
            CompareReport Unequal nfile nx -> do
                list <- fileFilter (fromMaybe file nfile) xs
                return (fromMaybe x nx:list)
    []     -> return []

updateCanonical :: FileData -> IO FileData
updateCanonical fileData = case maybeCanonical fileData of
    Just _   -> return fileData
    Nothing  -> do
        path <- canonicalizePath $ pathToFile fileData
        return $ fileData { maybeCanonical = Just path }

updateSize :: FileData -> IO FileData
updateSize fileData = case maybeSize fileData of
    Just _  -> return fileData
    Nothing -> do
        len <- withBinaryFile (pathToFile fileData) ReadMode hFileSize
        return $ fileData { maybeSize = Just len }

compareFile :: FileData -> FileData -> IO CompareReport
compareFile orig next = if pathToFile orig == pathToFile next
    then return $ CompareReport Identical Nothing Nothing
    else do
        c_orig <- updateCanonical orig
        c_next <- updateCanonical next
        if maybeCanonical c_orig == maybeCanonical c_next
            then return CompareReport
                { equal   = Identical
                , newOrig = diffDataFile orig c_orig
                , newNext = diffDataFile next c_next }
            else do
                s_orig <- updateSize c_orig
                s_next <- updateSize c_next
                if maybeSize s_orig /= maybeSize s_next
                    then return CompareReport
                        { equal   = Unequal
                        , newOrig = diffDataFile orig s_orig
                        , newNext = diffDataFile next s_next }
                    else do
                        origFile <- run_ $ enumFile (pathToFile s_orig) $$ consume
                        nextFile <- run_ $ enumFile (pathToFile s_next) $$ consume
                        if origFile == nextFile
                            then return CompareReport
                                { equal   = Equal
                                , newOrig = diffDataFile orig s_orig
                                , newNext = diffDataFile next s_next }
                            else return CompareReport
                                { equal   = Unequal
                                , newOrig = diffDataFile orig s_orig
                                , newNext = diffDataFile next s_next }

diffDataFile :: FileData -> FileData -> Maybe FileData
diffDataFile orig next = if orig == next
    then Nothing
    else Just next
