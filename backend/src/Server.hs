{-# LANGUAGE TypeOperators #-}

module Server where

import           Control.Monad.Trans      (liftIO)
import           Data.Time.Clock
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import qualified Vision                   as Vision

import           Api

examplePoems :: [Poem]
examplePoems =
  [ Poem "Sonnet IIIa" "ein Hauch auf nichts" [MkTag "de", MkTag "a"] [MkCollectionName "Rilke"]
  , Poem "Sonnet IIIb" "ein Wehn im Gott"     [MkTag "de", MkTag "b"] [MkCollectionName "Rainer Maria"]
  ]

-- handlers
-- we could make these be functions of DB, e.g.
-- https://github.com/haskell-servant/example-servant-elm/blob/50924b7acef84c29210a53fceaf6978e6048370f/server/src/App.hs

analyzePoem :: PoemInfo -> Handler PoemAnalysis
analyzePoem info = do
  results <- liftIO $ Vision.analyze $ gsURI info
  return $ PoemAnalysis results

allPoems :: Handler [Poem]
allPoems = return examplePoems

taggedPoems :: [Tag] -> Handler [Poem]
taggedPoems tags = return examplePoems

poemsInCollection :: CollectionName -> Handler [Poem]
poemsInCollection n = return examplePoems

apiServer :: Server Api
apiServer = taggedPoems
  :<|> poemsInCollection
  :<|> allPoems
  :<|> analyzePoem


api :: Proxy Api
api = Proxy

app :: Application
app = serve api apiServer

