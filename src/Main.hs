{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import DiaLib

main :: IO ()
main = mainWith oblong