{-# LANGUAGE FlexibleInstances #-}      -- also needed for lenses??
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields  #-} -- to not have to come up with a unique name for each field
{-# LANGUAGE MultiParamTypeClasses  #-} -- needed for makeFieldsNoPrefix
{-# LANGUAGE FunctionalDependencies #-} -- also needed for makeFieldsnoprefix




module Vision where

import GHC.Generics
import Control.Lens ((&), (.~), (<&>), (?~), (^.), (^..), makeLenses, makeFieldsNoPrefix, traverse)
import Control.Lens.Iso (non)
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Text (Text)
import System.IO
import Data.Scientific as Scientific
import qualified Data.Aeson.Lens as AesonLens
import qualified Network.Wreq as Wreq
import qualified Data.ByteString.Lazy.Char8 as LC


-- | REQUEST TYPES

type ImageURI = String

data VisionRequest = VisionRequest
  { uri  :: ImageURI
  }
  deriving Show

instance ToJSON VisionRequest where
  toJSON v = object [
    "requests" .= [
        object [
            "image" .= object [
                "source" .= object [
                    "imageUri" .= uri v
                    ]
                ]
            ,"features" .= [
                object [
                    "type" .= ("DOCUMENT_TEXT_DETECTION"::String)
                    ]
                ]
            ]
        ]
    ]

-- | RESPONSE TYPES

data CustomProperty = CustomProperty
  {
    _name :: Text
  , _value :: Text
  , _uint64Value :: Text
  } deriving (Show)

makeFieldsNoPrefix ''CustomProperty
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''CustomProperty

-- https://cloud.google.com/vision/docs/reference/rest/v1/AnnotateImageResponse#EntityAnnotation
data EntityAnnotation = EntityAnnotation
  {
    _mid :: Maybe Text -- opaque entity id
  , _locale :: Maybe Text
  , _description :: Text
  , _score :: Maybe Scientific
  -- , _confidence :: Scientific -- deprecated, use score instead
  , _topicality :: Maybe Scientific
  -- ignoring these fields for now:
  -- , _boundingPoly :: BoundingPoly
  -- , _locations :: [LocationInfo]
  , _properties :: Maybe [CustomProperty]
  } deriving (Show)

makeFieldsNoPrefix ''EntityAnnotation
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''EntityAnnotation

data DetectedLanguage = DetectedLanguage
  {
    _languageCode :: Text
  , _confidence   :: Maybe Scientific
  } deriving Show

makeFieldsNoPrefix ''DetectedLanguage
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''DetectedLanguage


-- TODO: make this particular parsing work! Skipping for now.
data BreakType = Unknown
               | Space
               | SureSpace
               | EOLSureSpace
               | Hyphen
               | LineBreak
               deriving (Show, Generic)

-- https://stackoverflow.com/questions/27076491/aeson-parse-enumerated-data-types
createBreakType :: Text -> BreakType
createBreakType "UNKNOWN" = Unknown
createBreakType "SPACE"   = Space
createBreakType "SURE_SPACE" = SureSpace
createBreakType "EOL_SURE_SPACE" = EOLSureSpace
createBreakType "HYPHEN" = Hyphen
createBreakType "LINE_BREAK" = LineBreak
createBreakType _            = Unknown

instance FromJSON BreakType
instance ToJSON   BreakType

data DetectedBreak = DetectedBreak
  {
    _breakType :: BreakType
  , _isPrefix :: Maybe Bool
  } deriving (Show, Generic)

makeFieldsNoPrefix ''DetectedBreak


-- https://artyom.me/aeson#postprocessing
instance FromJSON DetectedBreak where
  parseJSON = withObject "DetectedBreak" $ \o -> do
    breakTypeS <- o .: "type"
    isPrefix   <- o .:? "isPrefix"
    let breakType = createBreakType breakTypeS
    return $ DetectedBreak breakType isPrefix
instance ToJSON DetectedBreak

data TextProperty = TextProperty
  {
    _detectedLanguages :: [DetectedLanguage]
  , _detectedBreak     :: Maybe DetectedBreak
  } deriving (Show)

makeFieldsNoPrefix ''TextProperty
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''TextProperty


data Symbol = Symbol
  {
    _property :: Maybe TextProperty
    -- _boundingBox :: BoundingPoly
  , _text :: Text
  , _confidence :: Maybe Scientific
  } deriving (Show)

makeFieldsNoPrefix ''Symbol
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Symbol

data TextWord = TextWord
  {
    _property :: Maybe TextProperty
  -- , _boundingBox :: BoundingPoly
  , _symbols :: [Symbol]
  , _confidence :: Maybe Scientific
  } deriving (Show)

makeFieldsNoPrefix ''TextWord
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''TextWord

data Paragraph = Paragraph
  {
    _property :: Maybe TextProperty
  -- , _boundingBox :: BoundingPoly
  , _words :: [TextWord]
  , _confidence :: Maybe Scientific
  } deriving (Show)

makeFieldsNoPrefix ''Paragraph
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Paragraph

data Block = Block
  {
    _property :: Maybe TextProperty
  -- , _boundingBox :: BoundingPoly
  , _paragraphs :: [Paragraph]
  , _blockType :: Text -- BlockType
  , _confidence :: Maybe Scientific
  } deriving (Show)

data BlockType = UnknownBlockType
               | TextBlock
               | TableBlock
               | PictureBlock
               | RulerBlock
               | BarcodeBlock

makeFieldsNoPrefix ''Block
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Block

data Page = Page
  {
    _property :: TextProperty
  , _width    :: Scientific
  , _height   :: Scientific
  , _blocks   :: [Block]
  , _confidence :: Maybe Scientific
  } deriving (Show)

makeFieldsNoPrefix ''Page
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Page

-- This one would have the whole thing:
--  jq '.responses | .[0] | .fullTextAnnotation | .text '  sample-response-rest.json

data TextAnnotation = TextAnnotation
  {
    _pages :: [Page]
  , _text  :: Text
  } deriving (Show)

makeFieldsNoPrefix ''TextAnnotation
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''TextAnnotation


-- https://cloud.google.com/vision/docs/reference/rest/v1/AnnotateImageResponse

data AnnotateImageResponse =  AnnotateImageResponse
  {
    _fullTextAnnotation :: TextAnnotation
  , _textAnnotations    :: [EntityAnnotation]
  } deriving (Show)

makeFieldsNoPrefix ''AnnotateImageResponse
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''AnnotateImageResponse

data VisionAPIPayload = VisionAPIPayload
  {
    _responses :: [AnnotateImageResponse]
  } deriving (Show)

-- http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html
-- https://artyom.me/aeson#records-and-json-template-haskell
-- https://stackoverflow.com/questions/28123867/aeson-and-lens-with-derivegeneric-and-makelenses-names-dont-line-up
-- https://gist.github.com/mtesseract/1b69087b0aeeb6ddd7023ff05f7b7e68
makeFieldsNoPrefix ''VisionAPIPayload
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''VisionAPIPayload


-- | API INTERACTION FUNCTIONS


-- this is what `Wreq.asJSON =<< Wreq.postWith ...` returns
type AnnotateResponse = Wreq.Response (VisionAPIPayload)

annotateRemoteImage :: ImageURI -> IO AnnotateResponse
annotateRemoteImage uri = do
  let opts = Wreq.defaults
        & Wreq.header "X-Goog-Api-Key" .~ [""]
        & Wreq.header "Content-Type"   .~ ["application/json; charset=utf-8"]

  -- see: http://www.serpentine.com/wreq/tutorial.html#convenient-json-traversal
  r <- Wreq.postWith opts "https://vision.googleapis.com/v1/images:annotate"
       $ toJSON
       $ VisionRequest uri

  Wreq.asJSON r

-- | gets the full text of a given payload, for easier QA.
fullText :: VisionAPIPayload -> Text
fullText payload =
  head $ payload ^.. responses . traverse . fullTextAnnotation . text

-- | Get all paragraphs found across all pages (google calls them "blocks")
allBlocks :: VisionAPIPayload -> [Block]
allBlocks payload =
   payload
   ^.. responses . traverse
   . fullTextAnnotation
   . pages . traverse
   . blocks . traverse

allLines :: Block -> [Paragraph]
allLines block =
   block ^.. paragraphs . traverse

allWords :: Paragraph -> [TextWord]
allWords p =
  p ^.. Vision.words . traverse

type ConfidenceData = (Text, Scientific)

confidenceData :: TextWord -> ConfidenceData
confidenceData w =
  let w' = mconcat $ w ^.. symbols . traverse . text
      c  = w ^. confidence . non 0
  in (w', c)

data AnnotatedLine = AnnotatedLine
  {
    annotatedWords :: [ConfidenceData]
  } deriving (Show)

data AnnotatedBlock = AnnotatedBlock
  {
    lines :: [AnnotatedLine]
  } deriving (Show)

annotatedLines :: Block -> [AnnotatedLine]
annotatedLines p =
  [ AnnotatedLine $ map confidenceData $ allWords l | l <- allLines p ]

annotatedBlocks :: VisionAPIPayload -> [AnnotatedBlock]
annotatedBlocks payload =
  [ AnnotatedBlock $  annotatedLines p | p <- allBlocks payload ]

-- analyze :: ImageURI -> [ConfidenceData]
-- analyze uri = do
--  r <- annotateRemoteImage uri
--  let d = confidenceData 
      

