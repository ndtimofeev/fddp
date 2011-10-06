module Exceptions (Exception(..), tryBadIO) where

-- base
import System.IO.Error (tryIOError)

-- transformers
import Control.Monad.Trans.Error (ErrorT(..), Error(..))

data Exception
    = BadKey String
    | BadFile String
    | BadLogic String
    | BadIO IOError
    | Exception String
    deriving Show

tryBadIO :: IO a -> ErrorT Exception IO a
tryBadIO act = ErrorT $ do
    val <- tryIOError act
    return $ case val of
        Right x -> Right x
        Left v  -> Left $ BadIO v

instance Error Exception where
    strMsg = Exception
