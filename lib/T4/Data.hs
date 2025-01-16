module T4.Data where

import Data.Char
import qualified Data.Text as T
import Data.Time
import Data.Aeson
import Data.Aeson.TH

type Cat    = String
type Tag    = String

data Config = Config  { allCats :: [Cat]
                      , allTags :: [Tag]
                      }
              deriving (Show, Eq)

$(deriveJSON defaultOptions ''Config)

newtype SimpleLocalTime = SLT {getLocalTime :: LocalTime}
                          deriving (Eq, Show, FromJSON)

instance ToJSON SimpleLocalTime where
  toJSON = String . T.pack . fmt . getLocalTime
    where fmt = formatTime defaultTimeLocale "%F %T"

data Clock  = In  { time  :: SimpleLocalTime
                  , cat   :: Cat
                  , tags  :: [Tag]
                  }
            | Out { time  :: SimpleLocalTime
                  }
              deriving (Show, Eq)

$(deriveJSON defaultOptions
  { constructorTagModifier  = map toLower
  , sumEncoding             = ObjectWithSingleField
  } ''Clock)
