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



instance Parse Term where
  parser = Term <$> trailSpaces (many1 letter) <*> trailSpaces (some <|> pure [])
    where
      some = do
        withSpaces '('
        args <- sepBy parser $ withSpaces ','
        withSpaces ')'
        pure args



instance Parse Symbol where
  parser = do
      symb <- trailSpaces $ many1 letter
      withSpaces ':'
      next <- sepBy1 (trailSpaces $ many1 letter) (withSpaces 'x')
      let noArgs = Symbol symb [] (Type $ head next)
          args = do
            void $ trailSpaces $ string "->"
            res <- trailSpaces $ many1 letter
            pure $ Symbol symb (map Type next) (Type res)
      option noArgs args



instance Parse Signature where
  parser = Signature <$> many parser
