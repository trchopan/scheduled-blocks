
{-# LANGUAGE DeriveGeneric     #-}

module Domain.VrfSkey where


import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , object
                                                , withObject
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

data VrfSkey = VrfSkey
  { typeVrfSkey  :: Text
  , descriptionVrfSkey :: Text
  , cborHexVrfSkey     :: Text
  }
  deriving (Show, Generic)

instance ToJSON VrfSkey where
  toJSON p = object
    [ "type" .= typeVrfSkey p
    , "description" .= descriptionVrfSkey p
    , "cborHex" .= cborHexVrfSkey p
    ]

instance FromJSON VrfSkey where
  parseJSON = withObject "VrfSkey" $ \v ->
    VrfSkey <$> (v .: "type") <*> (v .: "description") <*> (v .: "cborHex")
