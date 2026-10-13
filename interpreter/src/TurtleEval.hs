module TurtleEval where
import TurtleParser
import RDF
import qualified Data.Map as Map

-- data URI =
--       FullURL String
--     | Postfix String
--     | Short String String
-- data Object = 
--       Uri URI
--     | LitStr String
--     | LitInt Int
-- data Expr =
--       Base String
--     | Prefix String String
--     | Triple URI URI Object
--     | NonTerminal Expr Expr
--     | Terminal Expr

data Env = Env 
    {
        base :: Maybe String,
        prefixes :: Map.Map String String
    }

emptyEnv :: Env
emptyEnv = Env
  { base = Nothing
  , prefixes = Map.empty
  }

evalUri :: URI -> String
evalUri (FullURL url) = "<" ++ url ++ ">"
evalUri other = error $ "evalUri received an unresolved URI: " ++ show other

evalObject :: Object -> String
evalObject (Uri u) = evalUri u
evalObject (LitStr str) = "\"" ++ str ++ "\""
evalObject (LitInt num) = show num
evalObject other = error $ "Unexpected input for evalObject: " ++ show other

evalTriples :: Triples -> [String]
evalTriples [] = []
evalTriples ((s, p, o):ts) =
    (evalUri s ++ " " ++ evalUri p ++ " " ++ evalObject o ++ " .") : evalTriples ts

-- interpret turtle

turtleIntr :: Expr -> Triples
turtleIntr ast = snd (resolveExpr emptyEnv ast)

resolveURI :: Env -> URI -> URI
resolveURI env uri =
    case uri of
        FullURL _ -> uri
        Postfix str ->
            case base env of
                Just b -> FullURL (b ++ str)
                Nothing -> error "No base defined for relative URI"
        Short prefix name ->
            case Map.lookup prefix (prefixes env) of
                Just p -> FullURL (p ++ name)
                Nothing -> error ("Unknown prefix: " ++ prefix)

resolveObject :: Env -> Object -> Object
resolveObject env (Uri u) = Uri (resolveURI env u)
resolveObject _ (LitStr str) = LitStr str
resolveObject _ (LitInt num) = LitInt num

resolveObj :: Env -> Obj -> [Object]
resolveObj env (SingularObj o) = [resolveObject env o]
resolveObj env (MultipleObj o os) = resolveObject env o:resolveObj env os

resolvePredObj :: Env -> PredObj -> [(URI, Object)]
resolvePredObj env (SingularPredObj p o) = 
    let predicate = resolveURI env p
        objectList = resolveObj env o 
    in map (\obj -> (predicate, obj)) objectList
resolvePredObj env (MultiplePredObj p o po) = resolvePredObj env (SingularPredObj p o) ++ resolvePredObj env po

resolveExpr :: Env -> Expr -> (Env, Triples)
resolveExpr env (Base b) = (env { base = Just b }, [])
resolveExpr env (Prefix name uri) = (env { prefixes = Map.insert name uri (prefixes env) }, [])
resolveExpr env (Triple s po) =
    let s' = resolveURI env s
        predObjList = resolvePredObj env po
        triples = map (\(p, o) -> (s', p, o)) predObjList
    in (env, triples)
resolveExpr env (NonTerminal e1 e2) = 
    let (env1, t1) = resolveExpr env e1 
        (env2, t2) = resolveExpr env1 e2
    in (env2, t1 ++ t2)
resolveExpr env (Terminal e) = resolveExpr env e