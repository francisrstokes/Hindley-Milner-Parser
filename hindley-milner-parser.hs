module HindleyMilnerSignatureParser (
  TExpr(TIdentifier,TExpressions,TTuple,TNestedType,TBracketed,TArrayOf),
  parseHindleyMilner
) where

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec
import Text.Parsec.Char

type TExpressionName = String

data TTypeClassClause = TClassConstraint (String, String)
                      | TClassConstraints [TTypeClassClause]
                      deriving (Show, Eq)

data TExpr = TIdentifier String
           | TExpressions [TExpr]
           | TTuple [TExpr]
           | TNestedType [TExpr]
           | TBracketed (TExpr)
           | TArrayOf (TExpr)
           deriving (Show, Eq)

type HindleyMilnerSignature = (TExpressionName, TTypeClassClause, TExpr)

-- Helper function to "extract" array types with only one element
extractable :: ([a] -> a) -> [a] -> a
extractable t a = if (length a > 1) then t a else head a

lowerStartingString = (many1 lower) <> (many alphaNum)
upperStartingString = (many1 upper) <> (many alphaNum)
letterStartingString = (many1 letter) <> (many alphaNum)
bracketed = between (char '(') (char ')')

-- The basic unit of a HM expression is either an identifier or a bracketed expression
hmUnit = do
  nested <- (identifier <|> hmBracketedExpr <|> hmArrayOf) `sepEndBy` spaces
  return $ (extractable TNestedType nested)

-- Multiple expressions are just a units separated by arrows
hmExpressions = do
  exprs <- (hmUnit `sepBy` hmArrow)
  return $ (extractable TExpressions exprs)

-- Single type class constraint
hmTypeClassConstraint = do
  typeClass <- upperStartingString
  spaces
  variable <- lowerStartingString
  return $ TClassConstraint (typeClass, variable)

-- Multiple type class constraints
hmTypeClassConstraints = TClassConstraints <$> bracketed (hmTypeClassConstraint `sepBy` hmComma)

-- The entire type class constraint clause
hmConstraints = do
  constraints <- hmTypeClassConstraint <|> hmTypeClassConstraints
  hmConstraintArrow
  return constraints

-- The hasType operator ::
hmHasType = spaces *> (string "::") *> spaces

-- The arrow operator
hmArrow = spaces *> (string "->") *> spaces

-- The fat arrow, separating the hasType operator and type expression
hmConstraintArrow = spaces *> string "=>" *> spaces

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
expressionName = lowerStartingString

-- Parser for the whole signature
hmParser = do
  exprName <- expressionName
  hmHasType
  constraints <- option (TClassConstraints []) (try hmConstraints)
  hmBody <- hmExpressions
  return (exprName, constraints, hmBody)


parseHindleyMilner :: String -> Either ParseError HindleyMilnerSignature
parseHindleyMilner s = parse hmParser [] s

main :: IO (Either ParseError HindleyMilnerSignature)
main =
  pure $ parseHindleyMilner "traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)"
