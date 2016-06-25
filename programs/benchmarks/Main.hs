{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.DeepSeq
import Criterion.Main
import Data.List (intersperse)
import qualified Data.Text as T
import Data.Text.Zipper

groups :: [Benchmark]
groups =
    [ textBenchmarks
    ]

textBenchmarks :: Benchmark
textBenchmarks =
    let initial = force $ moveCursor (initRow, initCol) $ textZipper content Nothing
        h = 50 -- in lines
        w = 10 -- in words
        ws = ["foo", "bar", "things", "stuff"]
        content = replicate h line
        line = T.concat $ intersperse " " $ take w $ concat $ repeat ws
        initRow = h `div` 2
        initCol = (T.length line) `div` 2
    in bgroup "text"
        [ bench "moveLeft"       $ nf moveLeft         initial
        , bench "moveRight"      $ nf moveRight        initial
        , bench "moveUp"         $ nf moveUp           initial
        , bench "moveDown"       $ nf moveDown         initial
        , bench "deleteChar"     $ nf deleteChar       initial
        , bench "deletePrevChar" $ nf deletePrevChar   initial
        , bench "gotoBOL"        $ nf gotoBOL          initial
        , bench "gotoEOL"        $ nf gotoEOL          initial
        , bench "killToEOL"      $ nf killToEOL        initial
        , bench "breakLine"      $ nf breakLine        initial
        , bench "insertChar"     $ nf (insertChar 'x') initial
        ]

main :: IO ()
main = defaultMain groups
