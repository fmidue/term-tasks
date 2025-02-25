{-# LANGUAGE FlexibleInstances #-}

module Parser where


import DataType

import Control.Monad (void)
import Text.ParserCombinators.Parsec




class Parse a where
  parser :: Parser a




trailSpaces :: Parser a -> Parser a
trailSpaces p = p <* spaces

withSpaces :: Char -> Parser ()
withSpaces = void . trailSpaces . char



instance Parse (Term String) where
  parser = Term <$> trailSpaces (many1 letter) <*> trailSpaces (some <|> pure [])
    where
      some = do
        withSpaces '('
        args <- sepBy parser $ withSpaces ','
        withSpaces ')'
        pure args



instance Parse Symbol where
  parser = do
      parsedSymbol <- trailSpaces $ many1 letter
      withSpaces ':'
      next@(t:_) <- sepBy1 (trailSpaces $ many1 letter) (withSpaces 'x')
      let noArgs = Symbol parsedSymbol [] (Type t)
          args = do
            void $ trailSpaces $ string "->"
            res <- trailSpaces $ many1 letter
            pure $ Symbol parsedSymbol (map Type next) (Type res)
      option noArgs args



instance Parse Signature where
  parser = Signature <$> parser



instance Parse a => Parse [a] where
  parser = (trailSpaces listParse <?> "List") <|> fail (
      "Could not parse a list of values: " ++
      "The elements of a list are enclosed by square brackets '[ ]' and separated by commas.")
    where
      listParse = do
        withSpaces '[' <|> fail "could not parse an opening '['"
        xs <- parser `sepBy` (withSpaces ',' <|> fail "parsed a wrong separator: Lists are comma-separated.")
        withSpaces ']' <|> fail "could not parse an enclosing ']'"
        pure xs
