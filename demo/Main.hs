{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Codec.Picture
       (Image (imageHeight, imageWidth), convertRGBA8, pixelAt, readImage,
       writePng)
import Codec.Picture.Dither.SA (Options (..), defaultOptions, dither)
import Codec.Picture.Grayscale (monochrome)
import Codec.Picture.ScaleDCT  (scaleWithKernel)
import Control.Applicative     (optional, (<**>))
import Control.Monad           (forM)
import Data.Bits               (clearBit, setBit)
import Data.Char               (chr)
import Data.Function           ((&))
import System.Exit             (exitFailure)

import qualified Codec.Picture.Dither.FS as FS
import qualified Options.Applicative as O

data Opts = Opts
    { optsInput      :: FilePath
    , optsOutput     :: Maybe FilePath
    , optsIterations :: Int
    , optsAlgorithm  :: Algo
    , optsMode       :: Mode
    }
  deriving (Show)

data Algo
    = FS
    | SA
    | FSSA
  deriving (Show, Read)

data Mode
    = Braille
    | Box
  deriving (Eq, Show, Read)

main :: IO ()
main = do
    Opts {..} <- O.execParser opts
    let options = defaultOptions { optionsIterationC = optsIterations }

    case optsOutput of
        Nothing -> processImage2 optsAlgorithm optsMode options optsInput
        Just o  -> processImage  optsAlgorithm          options optsInput o
  where
    opts = O.info (optsP <**> O.helper) $ mconcat
        [ O.fullDesc
        , O.progDesc "Dither your images"
        , O.header "juicydither - dither the images"
        ]

optsP :: O.Parser Opts
optsP = Opts
    <$> O.strArgument (O.metavar "INPUT" <> O.help "Input image")
    <*> optional (O.strArgument (O.metavar "OUTPUT" <> O.help "Output image"))
    <*> O.option O.auto (O.long "iterations" <> O.metavar "ITERS" <> O.help "How many iterations to perform (times the image size)" <> O.value 8 <> O.showDefault)
    <*> O.option O.auto (O.long "algorithm" <> O.metavar "ALGO" <> O.help "Algorithm" <> O.value SA <> O.showDefault)
    <*> O.option O.auto (O.long "mode" <> O.metavar "MODE" <> O.help "Console mode" <> O.value Braille <> O.showDefault)

processImage :: Algo -> Options -> FilePath -> FilePath -> IO ()
processImage algo options src dst = do
    eimg <- readImage src
    case eimg of
        Left err -> do
            print err
            exitFailure
        Right dimg -> do
            let img = convertRGBA8 dimg
            let mon = monochrome img
            let res = case algo of
                    SA   -> dither options mon
                    FS   -> FS.dither mon
                    FSSA -> dither options { optionsInitialImage = Just (FS.dither mon) } mon
            writePng dst res

processImage2 :: Algo -> Mode -> Options -> FilePath -> IO ()
processImage2 algo mode options src = do
    eimg <- readImage src
    case eimg of
        Left err -> do
            print err
            exitFailure

        Right dimg -> do
            let (maxW, maxH) = case mode of
                    Braille -> (140, 80)
                    Box     -> (70, 40)

            let img0 = convertRGBA8 dimg

            let img1 | imageWidth img0 <= maxH
                     , imageWidth img0 <= maxH
                     , mode /= Box
                     = img0

                     | otherwise =
                    let w = imageWidth img0
                        h = imageHeight img0

                        -- boxes need to be double wide
                        aspect :: Int
                        aspect = case mode of
                            Braille -> 1
                            Box     -> 2

                    in if h * maxW > maxH * w
                       then scaleWithKernel (aspect * w * maxH `div` h, maxH) kernel img0
                       else scaleWithKernel (aspect * maxW, h * maxW `div` w) kernel img0

            let w    = imageWidth img1
                h    = imageHeight img1

            let mon = monochrome img1
            let res = case algo of
                    SA   -> dither options mon
                    FS   -> FS.dither mon
                    FSSA -> dither options { optionsInitialImage = Just (FS.dither mon) } mon

            -- "rendering" helpers

            -- we assume black background
            let background :: Bool
                background = False

            let isWhite :: Int -> Int -> Bool
                isWhite x y
                    | x >= w    = background
                    | y >= h    = background
                    | otherwise = pixelAt res x y > 127

            let thisBitIs i True  x = setBit x i
                thisBitIs i False x = clearBit x i

            case mode of
                Braille -> do
                    ls <- forM [ 0 .. h `div` 4 ] $ \y -> forM [ 0.. w `div` 2 ] $ \x -> do
                        let off = 0
                                & thisBitIs 0 (isWhite (2 * x + 0) (4 * y + 0))
                                & thisBitIs 1 (isWhite (2 * x + 0) (4 * y + 1))
                                & thisBitIs 2 (isWhite (2 * x + 0) (4 * y + 2))
                                & thisBitIs 3 (isWhite (2 * x + 1) (4 * y + 0))
                                & thisBitIs 4 (isWhite (2 * x + 1) (4 * y + 1))
                                & thisBitIs 5 (isWhite (2 * x + 1) (4 * y + 2))
                                & thisBitIs 6 (isWhite (2 * x + 0) (4 * y + 3))
                                & thisBitIs 7 (isWhite (2 * x + 1) (4 * y + 3))

                        -- https://en.wikipedia.org/wiki/Braille_Patterns#Block
                        let character = chr $ 0x2800 + off
                        return character

                    putStrLn $ unlines ls

                Box -> do
                    let boxes :: [Char]
                        boxes =
                            [ ' '
                            , '▘'
                            , '▖'
                            , '▌'
                            , '▝'
                            , '▀'
                            , '▞'
                            , '▛'
                            , '▗'
                            , '▚'
                            , '▄'
                            , '▙'
                            , '▐'
                            , '▜'
                            , '▟'
                            , '█'
                            ]
                    ls <- forM [0 .. h `div` 2 ] $ \y -> forM [ 0 .. w `div` 2 ] $ \x -> do
                        let off = 0
                                & thisBitIs 0 (isWhite (2 * x + 0) (2 * y + 0))
                                & thisBitIs 1 (isWhite (2 * x + 0) (2 * y + 1))
                                & thisBitIs 2 (isWhite (2 * x + 1) (2 * y + 0))
                                & thisBitIs 3 (isWhite (2 * x + 1) (2 * y + 1))

                        let character = boxes !! off
                        return character

                    putStrLn $ unlines ls

kernel :: Int -> Int -> Double
kernel 0 0 = 1.75
kernel 0 1 = -0.125
kernel 1 0 = -0.125
kernel 1 1 = -0.0625
kernel _ _ = 0
