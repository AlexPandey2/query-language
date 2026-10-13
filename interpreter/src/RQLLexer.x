{
module RQLLexer where
}

%wrapper "posn"

$digit = [0-9]
$alpha = [a-zA-Z]

tokens :-

  $white+ ; 
  "--".* ;
  "#".*;

  "return"                          { \p _ -> PT p TokenReturn }
  "LOAD"                            { \p _ -> PT p TokenLoad }
  "UNION"                           { \p _ -> PT p TokenUnion }
  "INTERSECTION"                    { \p _ -> PT p TokenIntersection }
  "FILTER"                          { \p _ -> PT p TokenFilter }
  "FILTER NOT"                      { \p _ -> PT p TokenFilterNot }
  "AND"                             { \p _ -> PT p TokenAnd }
  "OR"                              { \p _ -> PT p TokenOr }
  "NOT"                             { \p _ -> PT p TokenNot }
  "JOIN"                            { \p _ -> PT p TokenJoin }
  "OPTIONAL JOIN"                   { \p _ -> PT p TokenOptionalJoin }
  "GROUP BY"                        { \p _ -> PT p TokenGroupBy }
  "MAX"                             { \p _ -> PT p TokenMax }
  "MIN"                             { \p _ -> PT p TokenMin }
  "COUNT"                           { \p _ -> PT p TokenCount }
  "SUM"                             { \p _ -> PT p TokenSum }
  "MULT"                            { \p _ -> PT p TokenMult }
  "AVG"                             { \p _ -> PT p TokenAvg }
  "DIFFERENCE"                      { \p _ -> PT p TokenDifference }
  "TRANSITIVE"                      { \p _ -> PT p TokenTransitive }
  "RENAME"                          { \p _ -> PT p TokenRename }
  "LIMIT"                           { \p _ -> PT p TokenLimit }
  "OFFSET"                          { \p _ -> PT p TokenOffset }

  "predicate"                       { \p _ -> PT p TokenPredicate }
  "object"                          { \p _ -> PT p TokenObject }
  "subject"                         { \p _ -> PT p TokenMatchSubject }
  "objSubj"                         { \p _ -> PT p TokenMatchObjectSubject }
  "obj"                             { \p _ -> PT p TokenMatchObject }
  "none"                            { \p _ -> PT p TokenNone }
  "S1"                              { \p _ -> PT p TokenS1 }
  "S2"                              { \p _ -> PT p TokenS2 }
  "P1"                              { \p _ -> PT p TokenP1 }
  "P2"                              { \p _ -> PT p TokenP2 }
  "O1"                              { \p _ -> PT p TokenO1 }
  "O2"                              { \p _ -> PT p TokenO2 }
  ","                               { \p _ -> PT p TokenComma }
  ";"                               { \p _ -> PT p TokenSemiColon }

  ">"                               { \p _ -> PT p TokenGt }
  ">="                              { \p _ -> PT p TokenGte }
  "<"                               { \p _ -> PT p TokenLt }
  "<="                              { \p _ -> PT p TokenLte }

  "="                               { \p _ -> PT p TokenEq }

  \<                                { \p _ -> PT p TokenLArrow }
  \>                                { \p _ -> PT p TokenRArrow }
  \(                                { \p _ -> PT p TokenLParen }
  \)                                { \p _ -> PT p TokenRParen }

  $digit+                           { \p s -> PT p (TokenInt (read s)) }
  \" [^\"]* \"                      { \p s -> PT p (TokenStrLit (init (drop 1 s))) } -- file paths or literals
  \< [^\>]* \>                      { \p s -> PT p (TokenURL (init (drop 1 s))) }    -- URIs in filters
  $alpha [$alpha $digit \_ \’]*     { \p s -> PT p (TokenVar s) } 


{
data RQLPosnToken = PT AlexPosn RQLToken deriving (Eq, Show)

--The token data type
data RQLToken
  = TokenReturn
  | TokenLoad
  | TokenUnion
  | TokenIntersection
  | TokenFilter
  | TokenFilterNot
  | TokenAnd
  | TokenOr
  | TokenNot
  | TokenJoin
  | TokenOptionalJoin
  | TokenGroupBy
  | TokenMax
  | TokenMin
  | TokenCount
  | TokenSum
  | TokenMult
  | TokenAvg
  | TokenDifference
  | TokenTransitive
  | TokenRename
  | TokenLimit
  | TokenOffset

  | TokenPredicate
  | TokenObject
  | TokenMatchSubject
  | TokenMatchObjectSubject
  | TokenMatchObject
  | TokenNone
  | TokenS1
  | TokenS2
  | TokenP1
  | TokenP2
  | TokenO1
  | TokenO2

  | TokenComma
  | TokenSemiColon
  | TokenGt
  | TokenGte
  | TokenLt
  | TokenLte
  | TokenEq

  | TokenLArrow
  | TokenRArrow
  | TokenLParen
  | TokenRParen

  | TokenInt Int
  | TokenStrLit String
  | TokenURL String
  | TokenVar String

  deriving (Eq, Show)
}
