{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec
import Text.Parsec.Char

data TExpr t  = TIdentifier String
              | TExpressionName String
              | TExpressions [TExpr t]
              | TTuple [TExpr t]
              | TNestedType [TExpr t]
              | TBracketed (TExpr t)
              | TArrayOf (TExpr t)
              deriving (Show, Eq)

type HindleyMilnerSignature t = (TExpr t, TExpr t)

-- Helper function to "extract" array types with only one element
extractable t a = if (length a > 1) then t a else head a

camelString = (many1 $ lower) <> (many $ alphaNum)
letterStartingString = (many1 $ letter) <> (many $ alphaNum)
bracketed = between (char '(') (char ')')

-- The basic unit of a HM expression is either an identifier or a bracketed expression
hmUnit = do
  nested <- (identifier <|> hmBracketedExpr <|> hmArrayOf) `sepEndBy` spaces
  return $ (extractable TNestedType nested)

-- Multiple expressions are just a units separated by arrows
hmExpressions = do
  exprs <- (hmUnit `sepBy` hmArrow)
  return $ (extractable TExpressions exprs)

-- The hasType operator ::
hmHasType = spaces *> (string "::") *> spaces

-- The arrow operator
hmArrow = spaces *> (string "->") *> spaces

-- The comma operator for tuples
hmComma = spaces *> (char ',') *> spaces

-- variable or type identifiers
identifier = TIdentifier <$> letterStartingString

-- Bracketed tuple
hmTuple = TTuple <$> bracketed (hmExpressions `sepBy1` hmComma)

-- Bracketed expression
hmBracketedExpr = (TBracketed <$> bracketed hmExpressions) <|> hmTuple

-- Array type
hmArrayOf = TArrayOf <$> (char '[' *> hmExpressions <* char ']')

-- Expression name
expressionName = TExpressionName <$> camelString

-- Parser for the whole signature
hmParser = do
  exprName <- expressionName
  hmHasType
  hmBody <- hmExpressions
  return (exprName, hmBody)

parseHindleyMilner :: String -> Either ParseError (HindleyMilnerSignature t)
parseHindleyMilner s = parse hmParser [] s

main =
  pure $ parseHindleyMilner "map :: a -> [a]"
  -- pure $ parseHindleyMilner "map :: (a -> b) -> f a -> f b"
