module HttpClient where

import Data.ByteString.Lazy
import Network.HTTP.Client

class Monad m => MonadHttpClient m where
    runHttpClient :: Request -> m ByteString
