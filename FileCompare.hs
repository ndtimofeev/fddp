module FileCompare
    ( CompareReport(..)
    , Equal(..)
    , FileData(..)
    , compareFile
    , equalSize
    , equalContent
    , identicalPath
    , identicalCPath
    , updateCanonical
    , updateSize ) where

import EitherT (EitherT(..), left)
import Exceptions (Exception(..), tryBadIO)

-- base
import System.IO (IOMode(..), withBinaryFile, hFileSize)

-- directory
import System.Directory (canonicalizePath)

-- enumerator
import Data.Enumerator (($$), run_)
import Data.Enumerator.Binary (enumFile, consume)

-- transformers
import Control.Monad.Trans.Error (ErrorT(..), throwError)

data FileData = FileData
    { maybeSize      :: Maybe Integer
    , maybeCanonical :: Maybe FilePath
    , pathToFile     :: FilePath
    } deriving (Show, Eq, Ord)

data Equal = Equal | Unequal | Identical deriving Show

data CompareReport = CompareReport
    { equal   :: Equal
    , newOrig :: Maybe FileData
    , newNext :: Maybe FileData
    } deriving Show

type Gen = (FileData, FileData)

compareFile ::
    FileData ->
    FileData ->
    (Gen -> EitherT (Equal, Gen) IO Gen) ->
    ErrorT Exception IO CompareReport
compareFile orig next method = do
    val <- tryBadIO $ runEitherT $ method (orig, next)
    case val of
        Right _                  -> throwError $ BadLogic "Unfinished compare"
        Left (x, (norig, nnext)) -> return $
            CompareReport x (smtNew orig norig) (smtNew next nnext)

smtNew :: Eq a => a -> a -> Maybe a
smtNew orig next = if orig == next
    then Nothing
    else Just next

identicalPath :: Gen -> EitherT (Equal, Gen) IO Gen
identicalPath gen@(orig, next) = if pathToFile orig == pathToFile next
    then left (Identical, gen)
    else return gen

identicalCPath :: Gen -> EitherT (Equal, Gen) IO Gen
identicalCPath (orig, next) = EitherT $ do
    c_orig <- updateCanonical orig
    c_next <- updateCanonical next
    return $ if maybeCanonical c_orig == maybeCanonical c_next
        then Left (Identical, (c_orig, c_next))
        else Right (c_orig, c_next)

updateCanonical :: FileData -> IO FileData
updateCanonical fileData = case maybeCanonical fileData of
    Just _   -> return fileData
    Nothing  -> do
        path <- canonicalizePath $ pathToFile fileData
        return $ fileData { maybeCanonical = Just path }

equalSize :: Gen -> EitherT (Equal, Gen) IO Gen
equalSize (orig, next) = EitherT $ do
    s_orig <- updateSize orig
    s_next <- updateSize next
    return $ if maybeSize s_orig == maybeSize s_next
        then Right (s_orig, s_next)
        else Left (Unequal, (s_orig, s_next))

equalContent :: Gen -> EitherT (Equal, Gen) IO Gen
equalContent gen@(orig, next) = EitherT $ do
    origFile <- run_ $ enumFile (pathToFile orig) $$ consume
    nextFile <- run_ $ enumFile (pathToFile next) $$ consume
    return $ Left $ ((if origFile == nextFile then Equal else Unequal), gen)



updateSize :: FileData -> IO FileData
updateSize fileData = case maybeSize fileData of
    Just _  -> return fileData
    Nothing -> do
        len <- withBinaryFile (pathToFile fileData) ReadMode hFileSize
        return $ fileData { maybeSize = Just len }
