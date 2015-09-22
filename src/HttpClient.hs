module HttpClient where

import Control.Lens
import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString.Lazy
import Network.Wreq

class Monad m => MonadHttpClient m where
    runHttpClient :: String -> ByteString -> m ByteString

instance MonadHttpClient m => MonadHttpClient (LoggingT m) where
    runHttpClient url body = lift $ runHttpClient url body

instance MonadHttpClient m => MonadHttpClient (ReaderT r m) where
    runHttpClient url body = lift $ runHttpClient url body

instance MonadHttpClient IO where
    runHttpClient url body = do
        r <- post url body
        return (r ^. responseBody)
