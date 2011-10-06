module Main where

import Exceptions (Exception(..))
import FileCompare
    ( CompareReport(..)
    , Equal(..)
    , FileData(..)
    , compareFile
    , equalContent
    , equalSize
    , identicalPath
    , identicalCPath )

-- base
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

-- directory
import System.Directory (doesFileExist)

-- transformers
import Control.Monad.Trans.Error (ErrorT(..), throwError)
import Control.Monad.Trans.Class (lift)

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
        ufiles <- fileFilter x xs
        process ufiles
    []     -> return ()

fileFilter :: FileData -> [FileData] -> ErrorT Exception IO [FileData]
fileFilter file files = {- fileFilterRec file files [] -} case files of
    (x:xs) -> do
        (CompareReport val nfile nx) <- compareFile file x $
            \y -> identicalPath y >>= identicalCPath >>= equalSize >>= equalContent
        case val of
            Identical -> fileFilter (fromMaybe file nfile) xs
            Equal     -> fileFilter (fromMaybe file nfile) xs
            Unequal   -> do
                list <- fileFilter (fromMaybe file nfile) xs
                return (fromMaybe x nx:list)
    []     -> return []
    where
        fileFilterRec :: FileData -> [FileData] -> [FileData] -> ErrorT Exception IO [FileData]
        fileFilterRec l_file l_files acc = case l_files of
            (x:xs) -> do
                CompareReport val nfile nx <- compareFile l_file x $
                    \y -> identicalPath y >>= identicalCPath >>= equalSize >>= equalContent
                case val of
                    Identical  -> fileFilterRec (fromMaybe l_file nfile) xs acc
                    Equal      -> fileFilterRec (fromMaybe l_file nfile) xs acc
                    Unequal    -> fileFilterRec (fromMaybe l_file nfile) xs (acc ++ [fromMaybe x nx])
            []     -> return acc
