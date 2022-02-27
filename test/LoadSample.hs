module LoadSample where
import           Data.Aeson                     ( eitherDecode )
import qualified Data.ByteString.Lazy          as LBS
import           TestSample                     ( TestSample() )

readTestSampleOrError :: IO TestSample
readTestSampleOrError = do
  samples <-
    eitherDecode <$> LBS.readFile "./test/test-example.json" :: IO
      (Either String TestSample)
  case samples of
    Left  err    -> error err
    Right sample -> return sample
