{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Parser
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Test.QuickCheck.All (quickCheckAll)
import Data.List (isPrefixOf)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

readExpr input = case parse symbol "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value"

spaces = skipMany1 space




















prop_MatchSpecialChar s =
    readExpr s == "Found value"
    || "No match: \"lisp\" " `isPrefixOf` readExpr s

runParserTests = $quickCheckAll
