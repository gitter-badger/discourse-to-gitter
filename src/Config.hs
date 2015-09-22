module Config where

-- component
import Gitter
-- global
import Control.Lens
import Data.Aeson.TH

data Config = Config { _config_room :: Room }
makeLenses ''Config

deriveFromJSON defaultOptions ''Config
