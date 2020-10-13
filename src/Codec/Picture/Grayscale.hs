module Codec.Picture.Grayscale where

import Codec.Picture

monochrome
    :: Image PixelRGBA8  -- ^ Input image
    -> Image Pixel8 -- ^ Output image
monochrome = pixelMap $ \(PixelRGBA8 r g b _) -> toWord8
    $ 0.299 * fromIntegral r
    + 0.587 * fromIntegral g
    + 0.114 * fromIntegral b
  where
    toWord8 :: Double -> Pixel8
    toWord8 w
        | w < 0     = 0
        | w > 255   = 255
        | otherwise = round w

-- | processImage "phadej.png" "phadej-mono.png"
processImage :: FilePath -> FilePath -> IO ()
processImage src dst = do
    eimg <- readImage src
    case eimg of
        Left err -> print err
        Right dimg -> do
            let img = convertRGBA8 dimg
            let res = monochrome img
            writePng dst res
