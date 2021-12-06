{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module DiaLib where
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

sierpinski :: Int -> Diagram B
sierpinski 1 = triangle 1
sierpinski n = s === (s ||| s) # centerX
    where s = sierpinski (n-1)

triangular :: Int -> Diagram B
triangular 1 = square 1 # fc gray
triangular n =  triangular (n-1) ===
                hcat (replicate n (square 1 # fc gray))

row :: Int -> Colour Double -> Diagram B
row x c = hcat (replicate x (square 1 # fc c))

grid :: Int -> Int -> Colour Double -> Diagram B
grid x y c = vcat (replicate y (row x c))

oblong :: Diagram B
oblong = triangular 10 # translateY (-1) <> grid 10 11 lightgray