

{-# LANGUAGE ViewPatterns #-}

module Data.AER.TimeFront (timeFront, extractWindows) where



import qualified Data.AER.Types as AER
import qualified Data.AER.DVS128 as AER

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import           Data.Thyme.Clock
import           Data.Thyme.Time

timeFront :: Foldable t 
          => t (AER.Event AER.Address)
          -> U.Vector (NominalDiffTime,NominalDiffTime)
timeFront es = foldl go initialFront es
  where initialFront = U.replicate (128*128) (-1,-1) 
        go front (AER.qview -> (AER.U,x,y,t)) = U.modify (updateU x y t) front
        go front (AER.qview -> (AER.D,x,y,t)) = U.modify (updateD x y t) front

updateU x y t v = do
    let index = fromIntegral x + fromIntegral y * 128
    (_, oldD) <- UM.read v index
    UM.write v index (t, oldD)

updateD x y t v = do
    let index = fromIntegral x + fromIntegral y * 128
    (oldU, _) <- UM.read v index
    UM.write v index (oldU, t)


extractWindows size es = go splitTs es
    where fstTs = AER.timestamp . head $ es
          splitTs = [fstTs+size,fstTs+2*size..]
          go _      [] = []
          go (t:ts) xs = current : go ts rest
            where (current,rest) = span ((< t) . AER.timestamp) xs
