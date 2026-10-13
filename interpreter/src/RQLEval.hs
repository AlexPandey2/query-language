module RQLEval where
import TurtleEval
import RQLLexer
import RQLParser
import TurtleLexer
import TurtleParser
import RDF
import Data.List (nub, sort, maximumBy, minimumBy, groupBy, intercalate, (\\), intersect)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map as Map
import System.Directory (doesFileExist)

-- data RQL =
--       Union RQL RQL
--     | LoadFoo
--     | LoadBar 

data QueryInstr 
    = IVar String
    | ILoad String
    | IUnion 
    | IIntersection
    | IFilter Pattern
    | IFilterNot Pattern
    | IAggregate Grouping Op Pattern
    | IJoin JoinType
    | ILOuterJoin JoinType
    | IDifference
    | ITransitive Pattern
    | IRename Target Target Target
    | ILimit Int
    | IOffset Int

rqlMain :: String -> IO String
rqlMain fileContents = do
    let tokens = RQLLexer.alexScanTokens fileContents
    let ast = RQLParser.parseCalc tokens
    rawTriples <- stmtEval ast Map.empty
    let finalTriples = sort (nub rawTriples)
    let formatted = map (\(s, p, o) -> evalUri s ++ " " ++ evalUri p ++ " " ++ evalObject o ++ " .") finalTriples
    return $ intercalate "\n" formatted

stmtEval :: Statement -> Map.Map String Triples -> IO Triples
stmtEval (Assignment name expr stmt) env = do
  val <- queryEval (queryIntr expr) env
  stmtEval stmt (Map.insert name val env)
stmtEval (Return expr) env = queryEval (queryIntr expr) env

queryIntr :: RQL -> [QueryInstr] 
queryIntr (Load path) = [ILoad path]
queryIntr (Var name) = [IVar name]
queryIntr (Union q1 q2) = queryIntr q1 ++ queryIntr q2 ++ [IUnion]
queryIntr (Intersection q1 q2) = queryIntr q1 ++ queryIntr q2 ++ [IIntersection]
queryIntr (Filter p q) = queryIntr q ++ [IFilter p]
queryIntr (FilterNot p q) = queryIntr q ++ [IFilterNot p]
queryIntr (Aggregate g op p q) = queryIntr q ++ [IAggregate g op p]
queryIntr (Join t q1 q2) = queryIntr q1 ++ queryIntr q2 ++ [IJoin t]
queryIntr (LOuterJoin t q1 q2) = queryIntr q1 ++ queryIntr q2 ++ [ILOuterJoin t]
queryIntr (Difference q1 q2) = queryIntr q1 ++ queryIntr q2 ++ [IDifference]
queryIntr (Transitive p q) = queryIntr q ++ [ITransitive p]
queryIntr (Rename ts tp to q) = queryIntr q ++ [IRename ts tp to]
queryIntr (Limit n q) = queryIntr q ++ [ILimit n]
queryIntr (Offset n q) = queryIntr q  ++ [IOffset n]

queryEval :: [QueryInstr] -> Map.Map String Triples -> IO Triples
queryEval instrs = queryEval' instrs []

queryEval' :: [QueryInstr] -> [Triples] -> Map.Map String Triples -> IO Triples
queryEval' [] [result] _ = return result
queryEval' [] _ _ = error "Invalid stack state"

queryEval' (IVar name : is) stack env =
  case Map.lookup name env of
    Just triples -> queryEval' is (triples : stack) env
    Nothing -> error $ "Undefined variable: " ++ name

queryEval' (ILoad path : is) stack env = do
  exists <- doesFileExist path
  if not exists
    then error $ "File not found: " ++ path
    else do
      contents <- readFile path
      let tokens = TurtleLexer.alexScanTokens contents
      let ast = TurtleParser.parseCalc tokens
      let triples = turtleIntr ast
      queryEval' is (triples : stack) env

queryEval' (IUnion : is) (t2 : t1 : stack) env =
  queryEval' is (nub (t1 ++ t2) : stack) env

queryEval' (IIntersection : is) (t2 : t1 : stack) env =
  queryEval' is (intersect t1 t2 : stack) env

queryEval' (IFilter p : is) (triples : stack) env =
  let filtered = filter (matchPattern p) triples
  in queryEval' is (filtered : stack) env

queryEval' (IFilterNot p : is) (triples : stack) env =
  let filtered = filter (not . matchPattern p) triples
  in queryEval' is (filtered : stack) env

queryEval' (IAggregate g op p : is) (triples : stack) env =
  let aggregated = applyAggregate g op (filter (matchPattern p) triples)
  in queryEval' is (aggregated : stack) env

queryEval' (IJoin jType : is) (t2 : t1 : stack) env =
  let joined = applyJoin jType t1 t2
  in queryEval' is (joined : stack) env

queryEval' (ILOuterJoin jType : is) (t2 : t1 : stack) env =
  let joined = applyLOuterJoin jType t1 t2
  in queryEval' is (joined : stack) env

queryEval' (IDifference : is) (t2 : t1 : stack) env =
  queryEval' is ((t1 \\ t2) : stack) env

queryEval' (ITransitive p : is) (triples : stack) env =
  queryEval' is (applyTransitive p triples : stack) env

queryEval' (IRename ts tp to : is) (triples : stack) env =
  let renamed = map (applyRename ts tp to) triples
  in queryEval' is (renamed : stack) env

queryEval' (ILimit n : is) (triples : stack) env =
  queryEval' is (applyLimit n triples : stack) env

queryEval' (IOffset n : is) (triples : stack) env = 
  queryEval' is (applyOffset n triples : stack) env

queryEval' _ _ _ = error "Invalid query"

-- pattern matching

matchPattern :: Pattern -> Triple -> Bool
matchPattern (SubjectEq uri) (s, _, _) = uri == s
matchPattern (PredicateEq uri) (_, p, _) = uri == p
matchPattern (ObjectEq obj) (_, _, o) = obj == o
matchPattern (ObjectComp op val) (_, _, LitInt n) = performCompare op val n
matchPattern SubjectEqObject (s, _, o) = Uri s == o 
matchPattern (Conjunction p1 p2) t = matchPattern p1 t && matchPattern p2 t
matchPattern (Disjunction p1 p2) t = matchPattern p1 t || matchPattern p2 t
matchPattern (Negation p) t = not (matchPattern p t)
matchPattern _ _ = False

performCompare :: CompareOp -> Int -> Int -> Bool
performCompare Gt val n = n > val
performCompare Gte val n = n >= val
performCompare Lt val n = n < val
performCompare Lte val n = n <= val

-- aggregation

applyAggregate :: Grouping -> Op -> Triples -> Triples
applyAggregate g op triples
  | null triples = []
  | otherwise = findAll g (getFunc op) triples

getFunc :: Op -> (Triples -> Triple)
getFunc Max = findMax
getFunc Min = findMin
getFunc Count = findCount
getFunc Sum = findSum
getFunc Mult = findMult
getFunc Avg = findAvg

findAll :: Grouping -> (Triples -> Triple) -> Triples -> Triples
findAll g func triples = 
    let sorted = case g of
                    GroupSubject -> sortBy (comparing (\(s, _, _) -> s)) triples
                    GroupPredicate -> sortBy (comparing (\(_, p, _) -> p)) triples
                    GroupObject -> sortBy (comparing (\(_, _, o) -> o)) triples
                    None -> triples
        groups = groupByGrouping g sorted
    in map func groups

groupByGrouping :: Grouping -> Triples -> [Triples]
groupByGrouping None triples = [triples]
groupByGrouping GroupSubject triples = groupBy (\(s1, _, _) (s2, _, _) -> s1 == s2) triples
groupByGrouping GroupPredicate triples = groupBy (\(_, p1, _) (_, p2, _) -> p1 == p2) triples
groupByGrouping GroupObject triples = groupBy (\(_, _, o1) (_, _, o2) -> o1 == o2) triples 

findMax :: Triples -> Triple
findMax = maximumBy (comparing (\(_, _, o) -> o))

findMin :: Triples -> Triple
findMin = minimumBy (comparing (\(_, _, o) -> o)) 

findCount :: Triples -> Triple
findCount [] = error "Triples cannot be empty"
findCount ts@((s, p, _):_) = (s, p, LitInt (length ts))

findSum :: Triples -> Triple
findSum [] = error "Triples cannot be empty"
findSum ts@((s, p, _):_) =
  let values = [ n | (_, _, LitInt n) <- ts ]
      total = sum values
  in (s, p, LitInt total)

findMult :: Triples -> Triple
findMult [] = error "Triples cannot be empty"
findMult ts@((s, p, _):_) =
  let values = [ n | (_, _, LitInt n) <- ts ]
      total = product values
  in (s, p, LitInt total)

findAvg :: Triples -> Triple
findAvg [] = error "Cannot average an empty group"
findAvg ts@((s, p, _):_) =
  let values = [ n | (_, _, LitInt n) <- ts ]
      len = length values
      total = sum values
      result 
        | len == 0 = 0 
        | otherwise = total `div` len
  in (s, p, LitInt result)
  
-- joins

applyJoin :: JoinType -> Triples -> Triples -> Triples
applyJoin (JoinMatch js1 js2 ts tp to) g1 g2 =
    [ (s', p', o') | t1 <- g1
                   , t2 <- g2
                   , valAtSlot js1 t1 == valAtSlot js2 t2
                   , let s' = selectURI ts t1 t2
                   , let p' = selectURI tp t1 t2
                   , let o' = selectObject to t1 t2
                   ]

applyLOuterJoin :: JoinType -> Triples -> Triples -> Triples
applyLOuterJoin (JoinMatch js1 js2 ts tp to) g1 g2 = 
    [ (s', p', o') | t1 <- g1
                   , t2 <- g2
                   , valAtSlot js1 t1 == valAtSlot js2 t2
                   , let s' = selectURI ts t1 t2
                   , let p' = selectURI tp t1 t2
                   , let o' = selectObject to t1 t2
                   ] ++
    [ t1 | t1 <- g1
         , not (any (\t2 -> valAtSlot js1 t1 == valAtSlot js2 t2) g2)
         ]

valAtSlot :: JoinSlot -> Triple -> Object
valAtSlot S (s, _, _) = Uri s
valAtSlot P (_, p, _) = Uri p
valAtSlot O (_, _, o) = o

selectURI :: JoinResult -> Triple -> Triple -> URI
selectURI S1 (s, _, _) _ = s
selectURI S2 _ (s, _, _) = s
selectURI P1 (_, p, _) _ = p
selectURI P2 _ (_, p, _) = p
selectURI O1 (_, _, Uri o) _ = o
selectURI O2 _ (_, _, Uri o) = o
selectURI res _ _ = error $ "Join Error: Literal " ++ show res ++ " cannot be moved to a Subject/Predicate"  

selectObject :: JoinResult -> Triple -> Triple -> Object
selectObject O1 (_, _, o) _ = o
selectObject O2 _ (_, _, o) = o
selectObject S1 (s, _, _) _ = Uri s
selectObject S2 _ (s, _, _) = Uri s
selectObject P1 (_, p, _) _ = Uri p
selectObject P2 _ (_, p, _) = Uri p

-- transitive

applyTransitive :: Pattern -> Triples -> Triples
applyTransitive pattern ts = transitiveBFS visited visited
  where visited = [ t | t <- ts, matchPattern pattern t ]

transitiveBFS :: Triples -> Triples -> Triples
transitiveBFS visited [] = visited
transitiveBFS visited ts
  | length newVisited == length visited = newVisited
  | otherwise = transitiveBFS newVisited ts
  where newVisited = nub (visited ++ 
                   [ (s1, p1, o2) 
                   | (s1, p1, o1) <- visited
                   , (s2, _, o2) <- ts
                   , o1 == Uri s2 ])

-- renaming/projection

applyRename :: Target -> Target -> Target -> Triple -> Triple
applyRename ts tp to (s, p, o) = 
  let s' = toURI ts (s, p, o)
      p' = toURI tp (s, p, o)
      o' = toObj to (s, p, o)
  in (s', p', o')

toURI :: Target -> Triple -> URI
toURI FromSubject (s, _, _)   = s
toURI FromPredicate (_, p, _) = p
toURI FromObject (_, _, Uri u) = u
toURI (ConstantURI u) _ = u
toURI _ _ = error "Attempted to move a Literal to a Subject/Predicate"

toObj :: Target -> Triple -> Object
toObj FromSubject (s, _, _)   = Uri s
toObj FromPredicate (_, p, _) = Uri p
toObj FromObject (_, _, o) = o
toObj (ConstantObj o) _ = o
toObj (ConstantURI u) _ = Uri u

-- limit + offset

applyLimit :: Int -> Triples -> Triples
applyLimit = take

applyOffset :: Int -> Triples -> Triples
applyOffset = drop

-- helpers

getInt :: Object -> Int
getInt (LitInt n) = n
getInt _ = 0

isInt :: Object -> Bool
isInt (LitInt _) = True
isInt _ = False