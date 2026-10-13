{
module TurtleLexer where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]

tokens :-

  $white+ ; 
  "--".* ; 
  "#".* ;

  "@base"                           { \_ -> TokenBase }
  "@prefix"                         { \_ -> TokenPrefix }
  $digit+                           { \s -> TokenInt (read s) } 

  \.                                { \_ -> TokenDot }
  \;                                { \_ -> TokenSemi }
  \,                                { \_ -> TokenComma }
  \:                                { \_ -> TokenColon }
  \"                                { \_ -> TokenQuote }
  \<                                { \_ -> TokenLArrow }
  \>                                { \_ -> TokenRArrow }
  \(                                { \_ -> TokenLParen }
  \)                                { \_ -> TokenRParen }

  https?\:\/\/[a-zA-Z0-9\.\/\-\_\#\?\:]+   { \s -> TokenURL s }
  \"[^\"]*\"                               { \s -> TokenStrLit (init (drop 1 s)) }
  $alpha [$alpha $digit \_ \-]*            { \s -> TokenStr s }

  

{
-- The Token data type
data TTLToken 
  = TokenBase
  | TokenPrefix
  | TokenInt Int
  | TokenURL String
  | TokenStrLit String
  | TokenStr String
  | TokenDot
  | TokenSemi
  | TokenComma
  | TokenColon
  | TokenQuote
  | TokenLArrow
  | TokenRArrow
  | TokenLParen
  | TokenRParen

  deriving (Eq, Show)
}