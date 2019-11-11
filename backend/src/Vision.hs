{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Vision where

import           Control.Lens               (makeFieldsNoPrefix, makeLenses,
                                             traverse, (&), (.~), (<&>), (?~),
                                             (^.), (^..))
import           Control.Lens.Iso           (non)
import           Data.Aeson
import qualified Data.Aeson.Lens            as AesonLens
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Scientific            as Scientific
import           Data.Text                  (Text)
import           GHC.Generics
import qualified Network.Wreq               as Wreq
import           System.IO


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
    _name        :: Text
  , _value       :: Text
  , _uint64Value :: Text
  } deriving (Show)

makeFieldsNoPrefix ''CustomProperty
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''CustomProperty

-- https://cloud.google.com/vision/docs/reference/rest/v1/AnnotateImageResponse#EntityAnnotation
data EntityAnnotation = EntityAnnotation
  {
    _mid         :: Maybe Text -- opaque entity id
  , _locale      :: Maybe Text
  , _description :: Text
  , _score       :: Maybe Scientific
  -- , _confidence :: Scientific -- deprecated, use score instead
  , _topicality  :: Maybe Scientific
  -- ignoring these fields for now:
  -- , _boundingPoly :: BoundingPoly
  -- , _locations :: [LocationInfo]
  , _properties  :: Maybe [CustomProperty]
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
createBreakType "UNKNOWN"        = Unknown
createBreakType "SPACE"          = Space
createBreakType "SURE_SPACE"     = SureSpace
createBreakType "EOL_SURE_SPACE" = EOLSureSpace
createBreakType "HYPHEN"         = Hyphen
createBreakType "LINE_BREAK"     = LineBreak
createBreakType _                = Unknown

instance FromJSON BreakType
instance ToJSON   BreakType

data DetectedBreak = DetectedBreak
  {
    _breakType :: BreakType
  , _isPrefix  :: Maybe Bool
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
    _property   :: Maybe TextProperty
    -- _boundingBox :: BoundingPoly
  , _text       :: Text
  , _confidence :: Maybe Scientific
  } deriving (Show)

makeFieldsNoPrefix ''Symbol
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Symbol

data TextWord = TextWord
  {
    _property   :: Maybe TextProperty
  -- , _boundingBox :: BoundingPoly
  , _symbols    :: [Symbol]
  , _confidence :: Maybe Scientific
  } deriving (Show)

makeFieldsNoPrefix ''TextWord
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''TextWord

data Paragraph = Paragraph
  {
    _property   :: Maybe TextProperty
  -- , _boundingBox :: BoundingPoly
  , _words      :: [TextWord]
  , _confidence :: Maybe Scientific
  } deriving (Show)

makeFieldsNoPrefix ''Paragraph
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Paragraph

data Block = Block
  {
    _property   :: Maybe TextProperty
  -- , _boundingBox :: BoundingPoly
  , _paragraphs :: [Paragraph]
  , _blockType  :: Text -- BlockType
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
    _property   :: TextProperty
  , _width      :: Scientific
  , _height     :: Scientific
  , _blocks     :: [Block]
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


-- | API INTERACTION FUNCTIONS AND TYPES


-- this is what `Wreq.asJSON =<< Wreq.postWith ...` returns
type AnnotateResponse = Wreq.Response (VisionAPIPayload)

data AnnotatedWord = AnnotatedWord
  { _text       :: Text
  , _confidence :: Scientific
  } deriving (Show, Generic)

deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''AnnotatedWord

data AnnotatedLine = AnnotatedLine
  {
    _annotatedWords :: [AnnotatedWord]
  } deriving (Show, Generic)

deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''AnnotatedLine

data AnnotatedBlock = AnnotatedBlock
  {
    _lines :: [AnnotatedLine]
  } deriving (Show, Generic)

deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''AnnotatedBlock


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


annotateWord :: TextWord -> AnnotatedWord
annotateWord w =
  let w' = mconcat $ w ^.. symbols . traverse . text
      c  = w ^. confidence . non 0
  in AnnotatedWord w' c

annotatedLines :: Block -> [AnnotatedLine]
annotatedLines p =
  [ AnnotatedLine $ map annotateWord $ allWords l | l <- allLines p ]

annotatedBlocks :: VisionAPIPayload -> [AnnotatedBlock]
annotatedBlocks payload =
  [ AnnotatedBlock $  annotatedLines p | p <- allBlocks payload ]

analyze :: ImageURI -> IO [AnnotatedBlock]
analyze uri = do
  r <- annotateRemoteImage uri
  return $ annotatedBlocks $ r ^. Wreq.responseBody


{-

λ> a <- analyze "gs://orpheus-ocr-artifacts/IMG_5013.jpg"
λ> take 1 a
[AnnotatedBlock {lines = [AnnotatedLine {annotatedWords = [("thering",0.96),("there",0.93),("I",0.94),("got",0.5),(",",0.86),("incredulas",0.95),(",",0.95),("a",0.95),("bit",0.92),("buzzed",0.91)]},AnnotatedLine {annotatedWords = [("some",0.91),("gry",0.74),("explan",0.85),("to",0.95),("rezegan",0.91)]}]}]


--parseConfigFile :: String ->

λ> a <- analyze "gs://orpheus-ocr-artifacts/IMG_5013.jpg"
λ> take 1 a
[AnnotatedBlock {lines = [AnnotatedLine {annotatedWords = [AnnotatedWord {_text = "thering", _confidence = 0.96},AnnotatedWord {_text = "there", _confidence = 0.93},AnnotatedWord {_text = "I", _confidence = 0.94},AnnotatedWord {_text = "got", _confidence = 0.5},AnnotatedWord {_text = ",", _confidence = 0.86},AnnotatedWord {_text = "incredulas", _confidence = 0.95},AnnotatedWord {_text = ",", _confidence = 0.95},AnnotatedWord {_text = "a", _confidence = 0.95},AnnotatedWord {_text = "bit", _confidence = 0.92},AnnotatedWord {_text = "buzzed", _confidence = 0.91}]},AnnotatedLine {annotatedWords = [AnnotatedWord {_text = "some", _confidence = 0.91},AnnotatedWord {_text = "gry", _confidence = 0.74},AnnotatedWord {_text = "explan", _confidence = 0.85},AnnotatedWord {_text = "to", _confidence = 0.95},AnnotatedWord {_text = "rezegan", _confidence = 0.91}]}]}]

-}
