{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}


module Api where

import           Data.Aeson
import           Data.Text
import           Data.Time    (UTCTime)
import           GHC.Generics
import           Servant.API
import qualified Vision       as Vision

{- Based on both:
   https://github.com/haskell-servant/example-servant-elm/blob/50924b7acef84c29210a53fceaf6978e6048370f/server/src/Api.hs
   and
   https://docs.servant.dev/en/stable/tutorial/ApiType.html

   For an explanation of generics and JSON:
   https://artyom.me/aeson

   And about From/TOHttpApiData:
   https://docs.servant.dev/en/stable/tutorial/Server.html#the-fromhttpapidata-tohttpapidata-classes
-}

type Api =
  "api" :>
  ( "poems" :> "tagged" :> QueryParams "tags" Tag :> Get '[JSON] [Poem] :<|>
    "poems" :> Capture "collection-name" CollectionName :> Get '[JSON] [Poem] :<|>
    "poems" :> "all" :> Get '[JSON] [Poem] :<|>
    "poems" :> "analyze" :> ReqBody '[JSON] PoemInfo :> Post '[JSON] PoemAnalysis
  )

-- See: https://guide.aelve.com/haskell/aeson-cookbook-amra6lk6#item-l2s3zzxi
-- for an explanation about the `deriving newtype` strategy for ToJSON
-- (trying to use `DeriveAnyClass` led to infinite loading (!))
newtype Tag = MkTag String
  deriving (Show, Eq, Ord, FromHttpApiData, ToHttpApiData)
  deriving newtype (ToJSON)

newtype CollectionName = MkCollectionName String
  deriving (Show, Eq, Ord, FromHttpApiData, ToHttpApiData)
  deriving newtype (ToJSON)

data Poem = Poem
  { title       :: String
  , body        :: String -- TODO: Text?
  , tags        :: [Tag]
  , collections :: [CollectionName]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Poem

data PoemInfo = PoemInfo
  { gsURI :: String }
  deriving (Show, Eq, Generic)

instance FromJSON PoemInfo
instance ToJSON PoemInfo

data PoemAnalysis = PoemAnalysis
  { paragraphs :: [Vision.AnnotatedBlock] }
  deriving (Show, Generic)

instance FromJSON PoemAnalysis
instance ToJSON PoemAnalysis


