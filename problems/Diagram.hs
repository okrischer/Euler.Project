{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine ( mainWith )
import DiaLib ( oblong )

main :: IO ()
main = mainWith oblong