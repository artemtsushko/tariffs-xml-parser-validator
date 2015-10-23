-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) Artem Tsushko, 2015
-- License     :  BSD3
--
-- Maintainer  :  artem.tsushko@gmail.com
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Main.hs"]


