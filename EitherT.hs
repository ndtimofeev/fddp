module EitherT (EitherT(..), left) where

-- base
import Control.Monad

-- transformers
import Control.Monad.Trans.Class

newtype EitherT l m r = EitherT { runEitherT :: m (Either l r) }

instance Monad m => Monad (EitherT l m) where
    return = EitherT . return . Right
    EitherT x >>= f = EitherT $ do
        ret <- x
        case ret of
            Right r -> runEitherT $ f r
            Left l  -> return $ Left l

instance MonadTrans (EitherT l) where
    lift = EitherT . liftM Right

left :: Monad m => l -> EitherT l m r
left = EitherT . return . Left
