{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
-- #define DITHER_DEBUG

-- | Black and white dithering using simulated annealing :)
module Codec.Picture.Dither.SA (
    dither,
    Options (..),
    defaultOptions,
) where

import Codec.Picture
import Data.Bits               (testBit)
import Data.Word               (Word64, Word8)
import Text.Printf             (printf)

import qualified Data.Primitive         as Prim
import qualified Data.Vector            as V
import qualified Data.Vector.Storable   as VS
import qualified System.Random.SplitMix as SM

#ifdef DITHER_DEBUG
import System.IO.Unsafe (unsafePerformIO)
#else
import Control.Monad.ST (ST, runST)
#endif

-------------------------------------------------------------------------------
-- Debug helper
-------------------------------------------------------------------------------

#ifdef DITHER_DEBUG
debug :: String -> IO ()
debug = putStrLn
#else
debug :: String -> ST s ()
debug _ = return ()
#endif

-------------------------------------------------------------------------------
-- Kernel
-------------------------------------------------------------------------------

-- | Precomputed indices and weights
data K = K !Int !Double
  deriving (Show)

data Kernel
    = Kernel9 !K !K !K !K !K !K !K !K !K -- ^ most
    | Kernel6 !K !K !K !K !K !K          -- ^ boundary
    | Kernel4 !K !K !K !K                -- ^ corner
  deriving (Show)

toKernel :: [K] -> Kernel
toKernel [a,b,c,d]           = Kernel4 a b c d
toKernel [a,b,c,d,e,f]       = Kernel6 a b c d e f
toKernel [a,b,c,d,e,f,g,h,i] = Kernel9 a b c d e f g h i
toKernel k                   = error $ "panic: Kernel of invalid size: " ++ show k

-- def normd(x,y): return 2.0 * ivar / np.pi * np.exp(-0.5 * ivar * (x*x+ y*y))
_kernel :: Int -> Int -> Double
_kernel 0 0 = 1.864259770697569    -- integrate.dblquad(normd, 0, 1, lambda x: 0, lambda x: 1)[0] * 4
_kernel 0 1 = 0.42956285411878575  -- integrate.dblquad(normd, 1, 3, lambda x: 0, lambda x: 1)[0] * 2
_kernel 1 0 = 0.42956285411878575
_kernel 1 1 = 0.09897989997908489  -- integrate.dblquad(normd, 1, 3, lambda x: 1, lambda x: 3)[0] * 1
_kernel _ _ = 0.0

kernel :: Int -> Int -> Double
kernel 0 0 = 0.46859173138000354
kernel 0 1 = 0.10797293634284622
kernel 1 0 = 0.10797293634284622
kernel 1 1 = 0.02487913081215289
kernel _ _ = 0.0

-------------------------------------------------------------------------------
-- Folding
-------------------------------------------------------------------------------

foldingM :: Monad m => s -> [a] -> (s -> a -> m s) -> m s
foldingM z xs0 f = go z xs0 where
    go !acc []     = return acc
    go !acc (x:xs) = f acc x >>= \acc' -> go acc' xs
{-# INLINE foldingM #-}

foldingK :: Monad m => s -> Kernel -> (s -> Int -> Double -> m s) -> m s
foldingK s0 (Kernel9 a b c d e f g h i) ff = do
    let gg (K ii dd) s = ff s ii dd
        {-# INLINE gg #-}
    s1 <- gg a s0
    s2 <- gg b s1
    s3 <- gg c s2
    s4 <- gg d s3
    s5 <- gg e s4
    s6 <- gg f s5
    s7 <- gg g s6
    s8 <- gg h s7
    gg i s8
foldingK s0 (Kernel6 a b c d e f) ff = do
    let gg (K ii dd) s = ff s ii dd
        {-# INLINE gg #-}
    s1 <- gg a s0
    s2 <- gg b s1
    s3 <- gg c s2
    s4 <- gg d s3
    s5 <- gg e s4
    gg f s5
foldingK s0 (Kernel4 a b c d) ff = do
    let gg (K ii dd) s = ff s ii dd
        {-# INLINE gg #-}
    s1 <- gg a s0
    s2 <- gg b s1
    s3 <- gg c s2
    gg d s3
{-# INLINE foldingK #-}

-------------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------------

data Options = Options
    { optionsPrngSeed :: !Word64 -- ^ Seed for random number generator
    , optionsIterationC  :: !Int
    }
  deriving Show

defaultOptions :: Options
defaultOptions = Options
    { optionsPrngSeed   = 0xFEEDBACC
    , optionsIterationC = 8
    }

-------------------------------------------------------------------------------
-- Dither algorithm
-------------------------------------------------------------------------------

-- simple threshold
dither :: Options -> Image Pixel8 -> Image Pixel8
dither Options {..} img = img
    { imageData = VS.generate len $ \i ->
        if Prim.indexPrimArray result i == 0 then 0 else 255
    }
  where
    w = imageWidth img
    h = imageHeight img

    -- also w * h
    len :: Int
    len = VS.length idata

    toIdx :: Int -> Int -> Int
    toIdx x y = x + y * w

    fromIdx :: Int -> (Int, Int)
    fromIdx i = let (y, x) = i `divMod` w in (x, y)

    maxIter :: Int
    maxIter = len * optionsIterationC

    idata :: VS.Vector Word8
    idata = imageData img

    ddata :: Prim.PrimArray Double
    ddata = Prim.generatePrimArray len $ \i -> fromIntegral (idata VS.! i) / 255.0

    -- this is derived using stetson-harrison method,
    -- specifically the coefficient 0.2
    --
    probability :: Int -> Double -> Double
    probability iter diff = 0.2 * diff * (1.0 - progress)
      where
        progress :: Double
        progress = fromIntegral iter / fromIntegral maxIter

    sq :: Double -> Double
    sq x = x * x
    {-# INLINE sq #-}

    scale :: Double -> Double
    scale x = x / fromIntegral len

    indicesV :: V.Vector Kernel
    indicesV = V.generate len $ \i -> toKernel
        [ K (toIdx x' y') weight
        | let (x,y) = fromIdx i
        , dx <- [-1,0,1]
        , dy <- [-1,0,1]
        , let x' = x + dx
        , let y' = y + dy
        , x' >= 0, y' >= 0
        , x' <  w, y' <  h
        , let weight = kernel (abs dx) (abs dy)
        ]

    result :: Prim.PrimArray Word8
#ifdef DITHER_DEBUG
    result = unsafePerformIO $ do
#else
    result = runST $ do
#endif
        -- PRNG
        let g0 = SM.mkSMGen optionsPrngSeed

        -- work vector
        work <- Prim.newPrimArray len

        let slowenergy = foldingM 0 [0..len-1] $ \acc i -> do
                let indices :: Kernel
                    indices = V.unsafeIndex indicesV i

                diff <- foldingK 0.0 indices $ \ !acc' j weight -> do
                    let a = Prim.indexPrimArray ddata j
                    b <- Prim.readPrimArray work j
                    return $! acc' + (a - fromIntegral b) * weight

                return $! acc + sq diff
            {-# NOINLINE slowenergy #-}

        let neighborEnergy j = {-# SCC neighborEnergy #-} foldingK 0 (V.unsafeIndex indicesV j) $ \acc i _ -> do
                let indices :: Kernel
                    indices = V.unsafeIndex indicesV i

                diff <- foldingK 0.0 indices $ \acc' k weight -> do
                    let a = Prim.indexPrimArray ddata k
                    b <- Prim.readPrimArray work k
                    return $! acc' + (a - fromIntegral b) * weight

                return $! acc + sq diff
            {-# INLINE neighborEnergy #-}

        -- random initialisation
        g1 <- foldingM g0 [0..len-1] $ \g i -> do
            let (w64, g') = SM.nextWord64 g
            Prim.writePrimArray work i (if testBit w64 0 then 1 else 0 :: Word8)
            return g'

        e0 <- slowenergy
        debug $ "post init: " ++ show (scale e0)

        _g2 <- foldingM (e0, g1) [0 .. maxIter] $ \(e, g) iter -> do
            let (iw, g') = SM.bitmaskWithRejection64 (fromIntegral len) g
            let i :: Int
                i = fromIntegral iw


            n <- neighborEnergy i

            -- flip
            z <- Prim.readPrimArray work i
            Prim.writePrimArray work i (if z == 0 then 1 else 0)
            n' <- neighborEnergy i

            (diff, e') <-
                if mod iter (len `div` 4) /= 0
                then let diff = n' - n in return (diff, e + diff)
                else do
                    e' <- slowenergy
                    let diff = e' - e
                    debug $ "iteration: " ++ show iter
                    debug $ printf "energy: %.6f; scaled difference %.6f %.6f, probability %.6f" (scale e') diff (n' - n) (probability iter diff)
                    return (diff, e')

            if diff < 0
            then return (e', g')
            else do
                let (guess, g'') = SM.nextDouble g
                if guess < probability iter diff
                then do
                    debug "Accept!"
                    return (e', g'')
                else do
                    -- unflip
                    Prim.writePrimArray work i z

                    -- continue
                    return (e, g'')

        -- Statistics
        eFinal <- slowenergy
        squares <- foldingM 0 [0..len-1] $ \acc i -> do
            let a = Prim.indexPrimArray ddata i
            b <- Prim.readPrimArray work i
            return $! acc + sq (a - fromIntegral b)

        debug $ printf "Final energy: %.6f" (scale eFinal)
        debug $ printf "Squares diff: %.6f" (scale squares)

        -- Freeze the work array
        Prim.unsafeFreezePrimArray work
