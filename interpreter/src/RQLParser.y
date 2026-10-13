{
module RQLParser where
import RQLLexer
import RDF
}

%name parseCalc Statement
%tokentype { RQLPosnToken }
%error { parseError }

%token
    "return"             { PT _ TokenReturn }
    "LOAD"               { PT _ TokenLoad }
    "UNION"              { PT _ TokenUnion }
    "INTERSECTION"       { PT _ TokenIntersection }
    "FILTER"             { PT _ TokenFilter }
    "FILTER NOT"         { PT _ TokenFilterNot }
    "AND"                { PT _ TokenAnd }
    "OR"                 { PT _ TokenOr }
    "NOT"                { PT _ TokenNot }
    "JOIN"               { PT _ TokenJoin }
    "OPTIONAL JOIN"      { PT _ TokenOptionalJoin }
    "GROUP BY"           { PT _ TokenGroupBy }
    "MAX"                { PT _ TokenMax }
    "MIN"                { PT _ TokenMin }
    "COUNT"              { PT _ TokenCount }
    "SUM"                { PT _ TokenSum }
    "MULT"               { PT _ TokenMult }
    "AVG"                { PT _ TokenAvg }
    "DIFFERENCE"         { PT _ TokenDifference }
    "TRANSITIVE"         { PT _ TokenTransitive }
    "RENAME"             { PT _ TokenRename }
    "LIMIT"              { PT _ TokenLimit }
    "OFFSET"             { PT _ TokenOffset }
    "predicate"          { PT _ TokenPredicate }
    "object"             { PT _ TokenObject }
    "subject"            { PT _ TokenMatchSubject }
    "objSubj"            { PT _ TokenMatchObjectSubject }
    "obj"                { PT _ TokenMatchObject }
    "none"               { PT _ TokenNone }
    "S1"                 { PT _ TokenS1 }
    "S2"                 { PT _ TokenS2 }
    "P1"                 { PT _ TokenP1 }
    "P2"                 { PT _ TokenP2 }
    "O1"                 { PT _ TokenO1 }
    "O2"                 { PT _ TokenO2 }
    ','                  { PT _ TokenComma }
    ';'                  { PT _ TokenSemiColon }
    '>'                  { PT _ TokenGt }
    ">="                 { PT _ TokenGte }
    '<'                  { PT _ TokenLt }
    "<="                 { PT _ TokenLte }
    '='                  { PT _ TokenEq }
    '('                  { PT _ TokenLParen }
    ')'                  { PT _ TokenRParen }
    string               { PT _ (TokenStrLit $$) }
    int                  { PT _ (TokenInt $$) }
    url                  { PT _ (TokenURL $$) }
    var                  { PT _ (TokenVar $$) }

%left "UNION"
%left "INTERSECTION"
%left "OPTIONAL JOIN"
%left "JOIN"
%left "FILTER NOT"
%left "FILTER"
%left "OR"
%left "AND"
%left "NOT"
%%
Statement : var '=' RQL ';' Statement                                   { Assignment $1 $3 $5 }
          | "return" RQL                                                { Return $2 }

RQL : "LOAD" '(' string ')'                                             { Load $3 } 
    | var                                                               { Var $1 }
    | RQL "UNION" RQL                                                   { Union $1 $3 }
    | RQL "INTERSECTION" RQL                                            { Intersection $1 $3 }
    | "FILTER" '(' Pattern ',' RQL ')'                                  { Filter $3 $5 }
    | "FILTER NOT" '(' Pattern ',' RQL ')'                              { FilterNot $3 $5 }
    | "JOIN" '(' JoinType ',' RQL ',' RQL ')'                           { Join $3 $5 $7 }
    | "OPTIONAL JOIN" '(' JoinType ',' RQL ',' RQL ')'                  { LOuterJoin $3 $5 $7 }
    | Op '(' Pattern ',' RQL ')'                                        { Aggregate None $1 $3 $5 }
    | "GROUP BY" '(' Grouping ',' Op '(' Pattern ',' RQL ')' ')'        { Aggregate $3 $5 $7 $9 }
    | "DIFFERENCE" '(' RQL ',' RQL ')'                                  { Difference $3 $5 }
    | "TRANSITIVE" '(' Pattern ',' RQL ')'                              { Transitive $3 $5 }
    | "RENAME" '(' Target ',' Target ',' Target ',' RQL ')'             { Rename $3 $5 $7 $9 } 
    | "LIMIT" '(' int ',' RQL ')'                                       { Limit $3 $5 }
    | "OFFSET" '(' int ',' RQL ')'                                      { Offset $3 $5 }
    | '(' RQL ')'                                                       { $2 }

Pattern : "subject" '=' url                     { SubjectEq (FullURL $3) }
        | "predicate" '=' url                   { PredicateEq (FullURL $3) }
        | "object" '=' Object                   { ObjectEq $3 }
        | "object" CompareOp int                { ObjectComp $2 $3 }
        | "subject" '=' "object"                { SubjectEqObject }
        | Pattern "AND" Pattern                 { Conjunction $1 $3 }
        | Pattern "OR" Pattern                  { Disjunction $1 $3 }
        | "NOT" Pattern                         { Negation $2 }
        | '(' Pattern ')'                       { $2 } 

CompareOp : '>'                                 { Gt }              
          | ">="                                { Gte }
          | '<'                                 { Lt }
          | "<="                                { Lte }

Object : url                                    { Uri (FullURL $1) }
       | string                                 { LitStr $1 }
       | int                                    { LitInt $1 }

JoinType : JoinSlot ',' JoinSlot ',' JoinResult ',' JoinResult ',' JoinResult { JoinMatch $1 $3 $5 $7 $9 }

JoinSlot : "subject"                            { S }
         | "predicate"                          { P }
         | "object"                             { O }

JoinResult : "S1"                               { S1 }
           | "S2"                               { S2 }
           | "P1"                               { P1 }
           | "P2"                               { P2 }
           | "O1"                               { O1 }
           | "O2"                               { O2 }

Op : "MAX"                                      { Max }
   | "MIN"                                      { Min }
   | "COUNT"                                    { Count }
   | "SUM"                                      { Sum }
   | "MULT"                                     { Mult }
   | "AVG"                                      { Avg }

Grouping : "none"                               { None }
         | "subject"                            { GroupSubject }
         | "predicate"                          { GroupPredicate }
         | "object"                             { GroupObject }

Target : "subject"                              { FromSubject }
       | "predicate"                            { FromPredicate }
       | "object"                               { FromObject }
       | url                                    { ConstantURI (FullURL $1) }
       | int                                    { ConstantObj (LitInt $1) }
       | string                                 { ConstantObj (LitStr $1) }

{
parseError :: [RQLPosnToken] -> a
parseError [] = error "Unknown Parse Error" 
parseError tokens = error $ "Parse Error at Line " ++ (show ln) ++ " Column " ++ (show col)
    where PT p t = head tokens
          AlexPn ch ln col = p

data Statement =
      Assignment String RQL Statement
    | Return RQL

data RQL =
      Load String
    | Var String
    | Union RQL RQL
    | Intersection RQL RQL
    | Filter Pattern RQL
    | FilterNot Pattern RQL
    | Aggregate Grouping Op Pattern RQL
    | Join JoinType RQL RQL
    | LOuterJoin JoinType RQL RQL
    | Difference RQL RQL
    | Transitive Pattern RQL
    | Rename Target Target Target RQL
    | Limit Int RQL
    | Offset Int RQL
    deriving Show

data Pattern =
      SubjectEq URI
    | PredicateEq URI   
    | ObjectGte Int
    | ObjectComp CompareOp Int     
    | ObjectEq Object 
    | SubjectEqObject
    | Conjunction Pattern Pattern
    | Disjunction Pattern Pattern
    | Negation Pattern
    deriving (Eq, Show)

data Op = 
      Max
    | Min 
    | Count
    | Sum
    | Mult
    | Avg
    deriving (Eq, Show)

data Grouping =
      None
    | GroupSubject
    | GroupPredicate
    | GroupObject
    deriving (Eq, Show)
}