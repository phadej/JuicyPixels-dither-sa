{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
-- #define DITHER_DEBUG

-- | Black and white dithering using Floyd-Steinberg alrogithm :)
module Codec.Picture.Dither.FS (
    dither
) where

import Codec.Picture
import Control.Monad    (forM_)
import Control.Monad.ST (ST, runST)
import Data.Int         (Int16)

import qualified Data.Primitive       as Prim
import qualified Data.Vector.Storable as VS

dither :: Image Pixel8 -> Image Pixel8
dither img = runST $ do
    -- copy image to work array, Int16 so we can accumulate errors
    work <- Prim.newPrimArray len
    forM_ [0..len-1] $ \i -> do
        Prim.writePrimArray work i (fromIntegral (VS.unsafeIndex (imageData img) i) :: Int16)

    forM_ [0..h-1] $ \y -> forM_ [0..w-1] $ \x -> do
        let i = toIdx x y
        p <- Prim.readPrimArray work i
        let q = if p > 127 then 255 else 0
        Prim.writePrimArray work i q

        let d = p - q
        let d1 = d * 7 `div` 16
        let d2 = d * 3 `div` 16
        let d3 = d * 5 `div` 16
        let d4 = d - d1 - d2 - d3

        modifyIndex work (x + 1) (y + 0) (+ d1)
        modifyIndex work (x - 1) (y + 1) (+ d2)
        modifyIndex work (x + 0) (y + 1) (+ d3)
        modifyIndex work (x + 1) (y + 1) (+ d4)

    -- copy from work array to new storable vector
    newData <- VS.generateM len $ \i -> do
        p <- Prim.readPrimArray work i
        return (fromIntegral p)

    -- return modified image
    return img { imageData = newData }

  where
    len = VS.length (imageData img)
    w = imageWidth img
    h = imageHeight img

    toIdx :: Int -> Int -> Int
    toIdx x y = x + y * w

    modifyIndex :: Prim.MutablePrimArray s Int16 -> Int -> Int -> (Int16 -> Int16) -> ST s ()
    modifyIndex work x y f
        |  x < 0
        || y < 0
        || x >= w
        || y >= h
        = return ()

        | otherwise = do
            let i = toIdx x y
            p <- Prim.readPrimArray work i
            Prim.writePrimArray work i (f p)
