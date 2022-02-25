module Repository.KeyFile where

import           Data.Aeson                     ( eitherDecode )
import qualified Data.ByteString.Lazy          as LBS
import           Data.Text                      ( unpack )
import           Domain.VrfSkey                 ( VrfSkey(cborHexVrfSkey) )



loadVrfSkey :: FilePath -> IO VrfSkey
loadVrfSkey filepath = do
  result <- eitherDecode <$> LBS.readFile filepath :: IO (Either String VrfSkey)
  case result of
    Left  err    -> error $ "cannot load vrf key " ++ err
    Right vrfKey -> return vrfKey

poolVrfSkey :: VrfSkey -> String
poolVrfSkey = drop 4 . unpack . cborHexVrfSkey
