module Main(main) where

import Hloc.Example(run)
import System.IO(stderr, hPutStrLn)

main :: IO ()
main = run >> hPutStrLn stderr "good night"
