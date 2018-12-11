# Hindley-Milner Type Signature Parser

This module lets you convert a Hindley-Milner type signature string like `traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)` to a typed `Either Error 3-tuple` representing the structure that looks like:

```
Right ( "traverse"
      , TClassConstraints [ TClassConstraint ( "Applicative"
                                             , "f" )
                          , TClassConstraint ( "Traversable"
                                               , "t" ) ]
      , TExpressions [ TBracketed ( TExpressions [ TIdentifier "a"
                                                 , TNestedType [ TIdentifier "f"
                                                               , TIdentifier "b" ] ] )
                     , TNestedType [ TIdentifier "t"
                                   , TIdentifier "a" ]
                     , TNestedType [ TIdentifier "f"
                                   , TBracketed ( TNestedType [ TIdentifier "t"
                                                              , TIdentifier "b" ] ) ] ] )
```

Where the first element of the tuple is the expression name, the second is the type class constraints, and the third is a tree representing the type expression.