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

import Text.XML.HaXml

-- This strange looking comment adds code only needed when running the
-- doctest tests embedded in the comments
-- $setup
-- >>> import Data.List (stripPrefix)

main :: IO ()
main = return ()

data Tariff = Tariff {}
