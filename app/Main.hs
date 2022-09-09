module Main (main) where

import Codec.Picture
import qualified Codec.Picture.Metadata as Meta
import Control.Arrow ((>>>))
import Control.Exception
import Control.Monad
import Data.Bifunctor
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Options.Applicative

main :: IO ()
main =
  execParser opts >>= loadJuicyWithSize >>= \case
    Right (picture, (w, h)) ->
      display
        (InWindow "Gloss" (fromIntegral w, fromIntegral h) (0, 0))
        white
        picture
    Left err ->
      throw err

file :: Parser String
file = argument str (metavar "FILE")

opts :: ParserInfo String
opts =
  info
    (file <**> helper)
    ( fullDesc
        <> progDesc "Show image in FILE"
        <> header "graphtest - a simple image viewer"
    )

fromDynamicImageWithSize :: (DynamicImage, Meta.Metadatas) -> Either GraphtestError (Picture, (Word, Word))
fromDynamicImageWithSize (image, meta) = do
  picture <- maybeToRight ImageConversionError $ fromDynamicImage image
  let width = fromJust $ Meta.lookup Meta.Width meta
      height = fromJust $ Meta.lookup Meta.Height meta
  pure (picture, (width, height))

loadJuicyWithSize :: FilePath -> IO (Either GraphtestError (Picture, (Word, Word)))
loadJuicyWithSize = readImageWithMetadata >>> fmap (first ImageLoadError >=> fromDynamicImageWithSize)

maybeToRight :: e -> Maybe a -> Either e a
maybeToRight e = maybe (Left e) Right

data GraphtestError
  = ImageLoadError String
  | ImageConversionError
  deriving (Show)

instance Exception GraphtestError
