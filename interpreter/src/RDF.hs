module RDF where

data URI = 
      FullURL String
    | Postfix String
    | Short String String
    deriving (Eq, Show)

data Object = 
      Uri URI
    | LitStr String
    | LitInt Int
    deriving (Eq, Show)

uriToFormat :: URI -> String
uriToFormat (FullURL s)   = "<" ++ s ++ ">"
uriToFormat (Postfix s)   = "<" ++ s ++ ">" 
uriToFormat (Short p s)   = "<" ++ p ++ ":" ++ s ++ ">"

instance Ord URI where
    compare u1 u2 = compare (uriToFormat u1) (uriToFormat u2)

instance Ord Object where
    compare (LitStr a) (LitStr b) = compare a b
    compare (LitInt a) (LitInt b) = compare a b
    compare (Uri a) (Uri b) = compare a b
    compare (LitStr _) _ = LT
    compare _ (LitStr _) = GT
    compare (LitInt _) (Uri _) = LT
    compare (Uri _) (LitInt _) = GT

data JoinResult = S1 | P1 | O1 | S2 | P2 | O2 
    deriving (Eq, Ord, Show)

data JoinSlot = S | P | O
    deriving (Eq, Ord, Show)

data JoinType = JoinMatch JoinSlot JoinSlot JoinResult JoinResult JoinResult
    deriving (Eq, Ord, Show)

type Triple = (URI, URI, Object)
type Triples = [Triple]

data CompareOp =
      Gt
    | Gte
    | Lt
    | Lte
    deriving (Eq, Show)

data Target = 
      FromSubject 
    | FromPredicate 
    | FromObject 
    | ConstantURI URI 
    | ConstantObj Object
    deriving (Eq, Show)

