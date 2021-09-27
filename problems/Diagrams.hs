{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Diagrams where
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine ( mainWith )
import DiaLib ( oblong )

main :: IO ()
main = mainWith oblong