-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Artem Tsushko, 2015
-- License     :  BSD3
--
-- Maintainer  :  artem.tsushko@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
--
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Control.Monad (mapM_)
import Data.Maybe  (fromJust)
import System.Environment (getArgs)
import System.IO (readFile)
import Text.XML.HaXml.Parse (xmlParse, dtdParse)
import Text.XML.HaXml.Posn (posInNewCxt,noPos)
import Text.XML.HaXml.Util (docContent, contentElem)
import Text.XML.HaXml.Validate (partialValidate)
import Text.XML.HaXml.XmlContent (fromXml)
import Tariffs

-- | Parses XML file and validates it over DTD
main :: IO ()
main = do
    (xmlPath : dtdPath: _) <- getArgs
    xmlContents <- readFile xmlPath
    dtdContents <- readFile dtdPath
    let parsed = xmlParse xmlPath xmlContents
        dtd = dtdParse dtdPath dtdContents
        xml = contentElem . docContent noPos $ parsed
        errors = partialValidate (fromJust dtd) xml
    if null errors
    then do
        putStrLn "Validation successful!\n"
        let Right (Tariffs tariffs) = fromXml parsed
        mapM_ print tariffs
    else do
        putStrLn "Validation encountered errors:"
        mapM_ putStrLn errors
    return ()
