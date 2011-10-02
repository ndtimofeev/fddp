module Main where

-- base
import Control.Exception () --(try)
import System.Environment (getArgs)
import System.Console.GetOpt () --(ArgOrder(..), ArgDescr(..), OptDescr(..), getOpt, usageInfo)

-- transformers
--import Control.Monad.Trans.Error (ErrorT(..))

main :: IO ()
main = do
    argv <- getArgs -- >>= compileOpt
    mapM_ putStrLn argv

{-
compileOpt :: [String] -> IO [String]
compileOpt argv = case getOpt Permute options argv of
    (opts, args, []) -> do
        opt <- foldM (\x f -> f x) defaultOption opts
        return (args, opt)
    (_, _, err)      -> throwError $
        SomeException $ ErrorCall "Неверные ключи" ++ "\n" ++ (concat err)
-}
