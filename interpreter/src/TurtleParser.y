{
module TurtleParser where
import TurtleLexer
import RDF
}

%name parseCalc Base
%tokentype { TTLToken }
%error { parseError }
%token
    "@base"   { TokenBase }
    "@prefix" { TokenPrefix }
    url       { TokenURL $$ }
    lit       { TokenStrLit $$ }
    string    { TokenStr $$ }
    int       { TokenInt $$ }
    '.'       { TokenDot }
    ','       { TokenComma }
    ':'       { TokenColon }
    ';'       { TokenSemi }
    '<'       { TokenLArrow }
    '>'       { TokenRArrow }
    '('       { TokenLParen }
    ')'       { TokenRParen }
    '\"'      { TokenQuote  }

-- %left Terminal
-- %left NonTerminal
%%
Base : ListExpr { $1 }

ListExpr : Expr '.' { Terminal $1 }
         | Expr '.' ListExpr { NonTerminal $1 $3 }

Expr : "@base" '<' url '>' { Base $3 }
     | "@prefix" string ':' '<' url '>' { Prefix $2 $5 }
     | URI PredObj { Triple $1 $2 }
 
URI : '<' url '>' { FullURL $2 }
    | '<' string '>' { Postfix $2 }
    | '<' '>' { Postfix "" }
    | string ':' string { Short $1 $3 }

Object : URI { Uri $1 }
       | lit { LitStr $1 } 
       | int { LitInt $1 }

Obj : Object { SingularObj $1 }
    | Object ',' Obj { MultipleObj $1 $3 }

PredObj : URI Obj { SingularPredObj $1 $2 }
        | URI Obj ';' PredObj { MultiplePredObj $1 $2 $4 }

{
parseError :: [TTLToken] -> a
parseError tokens = error $ "Parse Error: " ++ show tokens 

data Obj =
      SingularObj Object
    | MultipleObj Object Obj
    deriving Show

data PredObj =
      SingularPredObj URI Obj
    | MultiplePredObj URI Obj PredObj
    deriving Show

data Expr =
      Base String
    | Prefix String String
    | Triple URI PredObj
    | NonTerminal Expr Expr
    | Terminal Expr
    deriving Show 

}