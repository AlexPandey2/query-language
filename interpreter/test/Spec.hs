import Test.Hspec
import RQLParser
import RQLLexer
import TurtleParser
import TurtleLexer
import RDF
import RQLEval
import TurtleEval
import Data.List (intersect, (\\))



main :: IO ()
main = hspec $ do
    describe "RQL Lexer & Parser" $ do
        let getRQL (RQLParser.Return rql) = rql
            getRQL _ = error "Expected Return statement"

        it "parses a LOAD command" $ do
            let tokens = RQLLexer.alexScanTokens "return LOAD(\"foo\")"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Load \"foo\""

        it "parses a UNION between two LOAD commands" $ do
            let tokens = RQLLexer.alexScanTokens "return LOAD(\"foo\") UNION LOAD(\"bar\")"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Union (Load \"foo\") (Load \"bar\")"

        it "parses a FILTER command with an equality " $ do
            let tokens = RQLLexer.alexScanTokens "return FILTER(object = \"larry\", LOAD(\"baz\"))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Filter (ObjectEq (LitStr \"larry\")) (Load \"baz\")"

        it "parses a JOIN command matching by subject" $ do
            let tokens = RQLLexer.alexScanTokens "return JOIN(subject, subject, S1, P1, O2, LOAD(\"foo\"), LOAD(\"bar\"))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Join (JoinMatch S S S1 P1 O2) (Load \"foo\") (Load \"bar\")"

        it "parses a FILTER command with an integer >= condition" $ do
            let tokens = RQLLexer.alexScanTokens "return FILTER(object >= 21, LOAD(\"baz\"))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Filter (ObjectComp Gte 21) (Load \"baz\")"

        it "parses a JOIN command matching an object to a subject" $ do
            let tokens = RQLLexer.alexScanTokens "return JOIN(object, subject, S1, P1, O2, LOAD(\"a\"), LOAD(\"b\"))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Join (JoinMatch O S S1 P1 O2) (Load \"a\") (Load \"b\")"

        it "parses a JOIN command matching by object" $ do
            let tokens = RQLLexer.alexScanTokens "return JOIN(object, object, S1, P1, O2, LOAD(\"c\"), LOAD(\"d\"))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Join (JoinMatch O O S1 P1 O2) (Load \"c\") (Load \"d\")"

        it "parses a FILTER command with an equality" $ do
            let tokens = RQLLexer.alexScanTokens "return FILTER(object = 50, LOAD(\"baz\"))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Filter (ObjectEq (LitInt 50)) (Load \"baz\")"

        it "parses a MAX aggregate command with a pattern" $ do
            let tokens = RQLLexer.alexScanTokens "return MAX(predicate = <http://example.org/ont/price>, LOAD(\"quux\"))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Aggregate None Max (PredicateEq (FullURL \"http://example.org/ont/price\")) (Load \"quux\")"

        it "parses a FILTER command with an AND combination" $ do
            let tokens = RQLLexer.alexScanTokens "return FILTER(predicate = <http://example.org/ont/hasAge> AND object >= 21, LOAD(\"baz\"))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Filter (Conjunction (PredicateEq (FullURL \"http://example.org/ont/hasAge\")) (ObjectComp Gte 21)) (Load \"baz\")"

        it "parses a FILTER command targeting a specific predicate URI" $ do
            let tokens = RQLLexer.alexScanTokens "return FILTER(predicate = <http://example.org/ont/teaches>, LOAD(\"foo\"))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Filter (PredicateEq (FullURL \"http://example.org/ont/teaches\")) (Load \"foo\")"

        it "parses a left-nested UNION between three LOAD commands" $ do
            let tokens = RQLLexer.alexScanTokens "return LOAD(\"a\") UNION LOAD(\"b\") UNION LOAD(\"c\")"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Union (Union (Load \"a\") (Load \"b\")) (Load \"c\")"

        it "parses a right-nested UNION using brackets" $ do
            let tokens = RQLLexer.alexScanTokens "return LOAD(\"a\") UNION (LOAD(\"b\") UNION LOAD(\"c\"))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Union (Load \"a\") (Union (Load \"b\") (Load \"c\"))"

        it "parses a FILTER command with a URL object" $ do
            let tokens = RQLLexer.alexScanTokens "return FILTER(object = <http://example.org/uos>, LOAD(\"foo\"))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Filter (ObjectEq (Uri (FullURL \"http://example.org/uos\"))) (Load \"foo\")"

        it "parses a FILTER command with three AND conditions" $ do
            let tokens = RQLLexer.alexScanTokens "return FILTER(predicate = <http://p.com> AND object >= 10 AND object = 20, LOAD(\"foo\"))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Filter (Conjunction (Conjunction (PredicateEq (FullURL \"http://p.com\")) (ObjectComp Gte 10)) (ObjectEq (LitInt 20))) (Load \"foo\")"

        it "parses a JOIN command wrapping two FILTER subqueries" $ do
            let tokens = RQLLexer.alexScanTokens "return JOIN(subject, subject, S1, P1, O2, FILTER(object = 1, LOAD(\"a\")), FILTER(object = 2, LOAD(\"b\")))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Join (JoinMatch S S S1 P1 O2) (Filter (ObjectEq (LitInt 1)) (Load \"a\")) (Filter (ObjectEq (LitInt 2)) (Load \"b\"))"

        it "parses a MAX aggregate command over a FILTER" $ do
            let tokens = RQLLexer.alexScanTokens "return MAX(predicate = <http://p.com>, FILTER(object >= 5, LOAD(\"c\")))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Aggregate None Max (PredicateEq (FullURL \"http://p.com\")) (Filter (ObjectComp Gte 5) (Load \"c\"))"

        it "parses a FILTER command over a JOIN subquery" $ do
            let tokens = RQLLexer.alexScanTokens "return FILTER(object >= 10, JOIN(object, subject, S1, P1, O2, LOAD(\"a\"), LOAD(\"b\")))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Filter (ObjectComp Gte 10) (Join (JoinMatch O S S1 P1 O2) (Load \"a\") (Load \"b\"))"

        it "parses deeply nested JOIN commands" $ do
            let tokens = RQLLexer.alexScanTokens "return JOIN(object, object, S1, P1, O2, JOIN(subject, subject, S1, P1, O2, LOAD(\"a\"), LOAD(\"b\")), LOAD(\"c\"))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Join (JoinMatch O O S1 P1 O2) (Join (JoinMatch S S S1 P1 O2) (Load \"a\") (Load \"b\")) (Load \"c\")"

        it "parses a FILTER command with string matching containing spaces" $ do
            let tokens = RQLLexer.alexScanTokens "return FILTER(object = \"John Doe\", LOAD(\"a\"))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Filter (ObjectEq (LitStr \"John Doe\")) (Load \"a\")"

        it "parses a LOAD command wrapped in redundant brackets" $ do
            let tokens = RQLLexer.alexScanTokens "return (LOAD(\"a\"))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Load \"a\""

        it "parses a UNION where the left hand side is a JOIN in brackets" $ do
            let tokens = RQLLexer.alexScanTokens "return (JOIN(subject, subject, S1, P1, O2, LOAD(\"a\"), LOAD(\"b\"))) UNION LOAD(\"c\")"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Union (Join (JoinMatch S S S1 P1 O2) (Load \"a\") (Load \"b\")) (Load \"c\")"

        it "parses a MAX aggregate wrapping a UNION" $ do
            let tokens = RQLLexer.alexScanTokens "return MAX(predicate = <http://a.com>, LOAD(\"x\") UNION LOAD(\"y\"))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Aggregate None Max (PredicateEq (FullURL \"http://a.com\")) (Union (Load \"x\") (Load \"y\"))"

        it "parses a complex combined query with match object subject" $ do
            let tokens = RQLLexer.alexScanTokens "return JOIN(object, subject, S1, P1, O2, MAX(predicate = <http://p.com>, LOAD(\"1\")), FILTER(object >= 50, LOAD(\"2\")))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Join (JoinMatch O S S1 P1 O2) (Aggregate None Max (PredicateEq (FullURL \"http://p.com\")) (Load \"1\")) (Filter (ObjectComp Gte 50) (Load \"2\"))"

        it "parses a DIFFERENCE command" $ do
            let tokens = RQLLexer.alexScanTokens "return DIFFERENCE(LOAD(\"a\"), LOAD(\"b\"))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Difference (Load \"a\") (Load \"b\")"

        it "parses a TRANSITIVE command" $ do
            let tokens = RQLLexer.alexScanTokens "return TRANSITIVE(predicate = <http://p>, LOAD(\"a\"))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Transitive (PredicateEq (FullURL \"http://p\")) (Load \"a\")"

        it "parses a LIMIT command" $ do
            let tokens = RQLLexer.alexScanTokens "return LIMIT(10, LOAD(\"a\"))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Limit 10 (Load \"a\")"
            
        it "parses an OFFSET command" $ do
            let tokens = RQLLexer.alexScanTokens "return OFFSET(5, LOAD(\"a\"))"
            show (getRQL (RQLParser.parseCalc tokens)) `shouldBe` "Offset 5 (Load \"a\")"


    describe "Turtle Lexer & Parser" $ do
        it "parses an @base URI declaration" $ do
            let tokens = TurtleLexer.alexScanTokens "@base <http://example.org> ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "Terminal (Base \"http://example.org\")"

        it "parses an @prefix declaration" $ do
            let tokens = TurtleLexer.alexScanTokens "@prefix ont : <http://example.org/ont/> ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "Terminal (Prefix \"ont\" \"http://example.org/ont/\")"

        it "parses a triple using a prefix and a string literal object" $ do
            let tokens = TurtleLexer.alexScanTokens "<nmg> ont:name \"Nick\" ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "Terminal (Triple (Postfix \"nmg\") (SingularPredObj (Short \"ont\" \"name\") (SingularObj (LitStr \"Nick\"))))"

        it "parses a triple with postfix URIs" $ do
            let tokens = TurtleLexer.alexScanTokens "<x> <y> <z> ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "Terminal (Triple (Postfix \"x\") (SingularPredObj (Postfix \"y\") (SingularObj (Uri (Postfix \"z\")))))"

        it "parses a triple with an integer literal object" $ do
            let tokens = TurtleLexer.alexScanTokens "<x> <y> 10 ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "Terminal (Triple (Postfix \"x\") (SingularPredObj (Postfix \"y\") (SingularObj (LitInt 10))))"

        it "parses a triple using a full URL" $ do
            let tokens = TurtleLexer.alexScanTokens "<http://example.org/a> <http://example.org/b> <http://example.org/c> ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "Terminal (Triple (FullURL \"http://example.org/a\") (SingularPredObj (FullURL \"http://example.org/b\") (SingularObj (Uri (FullURL \"http://example.org/c\")))))"

        it "parses an object list separated by commas" $ do
            let tokens = TurtleLexer.alexScanTokens "<nmg> ont:teaches <comp2312> , <comp2322> ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "Terminal (Triple (Postfix \"nmg\") (SingularPredObj (Short \"ont\" \"teaches\") (MultipleObj (Uri (Postfix \"comp2312\")) (SingularObj (Uri (Postfix \"comp2322\"))))))"

        it "parses a predicate-object list separated by semicolons" $ do
            let tokens = TurtleLexer.alexScanTokens "<nmg> ont:name \"Nick\" ; ont:worksFor <uos> ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "Terminal (Triple (Postfix \"nmg\") (MultiplePredObj (Short \"ont\" \"name\") (SingularObj (LitStr \"Nick\")) (SingularPredObj (Short \"ont\" \"worksFor\") (SingularObj (Uri (Postfix \"uos\"))))))"

        it "parses two consecutive @base declarations" $ do
            let tokens = TurtleLexer.alexScanTokens "@base <http://a.com> . @base <http://b.com> ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "NonTerminal (Base \"http://a.com\") (Terminal (Base \"http://b.com\"))"

        it "parses two consecutive @prefix declarations" $ do
            let tokens = TurtleLexer.alexScanTokens "@prefix a: <http://a.com> . @prefix b: <http://b.com> ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "NonTerminal (Prefix \"a\" \"http://a.com\") (Terminal (Prefix \"b\" \"http://b.com\"))"

        it "parses a triple with a three-item comma separated object list" $ do
            let tokens = TurtleLexer.alexScanTokens "<x> <y> 1 , 2 , 3 ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "Terminal (Triple (Postfix \"x\") (SingularPredObj (Postfix \"y\") (MultipleObj (LitInt 1) (MultipleObj (LitInt 2) (SingularObj (LitInt 3))))))"

        it "parses a triple with a three-item semicolon separated predicate-object list" $ do
            let tokens = TurtleLexer.alexScanTokens "<x> <y> 1 ; <z> 2 ; <w> 3 ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "Terminal (Triple (Postfix \"x\") (MultiplePredObj (Postfix \"y\") (SingularObj (LitInt 1)) (MultiplePredObj (Postfix \"z\") (SingularObj (LitInt 2)) (SingularPredObj (Postfix \"w\") (SingularObj (LitInt 3))))))"

        it "parses a triple mixing commas and semicolons" $ do
            let tokens = TurtleLexer.alexScanTokens "<x> <y> 1 , 2 ; <z> 3 ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "Terminal (Triple (Postfix \"x\") (MultiplePredObj (Postfix \"y\") (MultipleObj (LitInt 1) (SingularObj (LitInt 2))) (SingularPredObj (Postfix \"z\") (SingularObj (LitInt 3)))))"

        it "parses a triple using purely short URIs" $ do
            let tokens = TurtleLexer.alexScanTokens "ex:sub ex:pred ex:obj ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "Terminal (Triple (Short \"ex\" \"sub\") (SingularPredObj (Short \"ex\" \"pred\") (SingularObj (Uri (Short \"ex\" \"obj\")))))"

        it "parses a triple mixing Full, Short, and Postfix URIs" $ do
            let tokens = TurtleLexer.alexScanTokens "<http://a.com> ex:pred <obj> ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "Terminal (Triple (FullURL \"http://a.com\") (SingularPredObj (Short \"ex\" \"pred\") (SingularObj (Uri (Postfix \"obj\")))))"

        it "parses a semicolon list with integer objects" $ do
            let tokens = TurtleLexer.alexScanTokens "<x> <y> 100 ; <z> 200 ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "Terminal (Triple (Postfix \"x\") (MultiplePredObj (Postfix \"y\") (SingularObj (LitInt 100)) (SingularPredObj (Postfix \"z\") (SingularObj (LitInt 200)))))"

        it "parses a string literal containing spaces and numbers" $ do
            let tokens = TurtleLexer.alexScanTokens "<x> <y> \"Hello World 123\" ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "Terminal (Triple (Postfix \"x\") (SingularPredObj (Postfix \"y\") (SingularObj (LitStr \"Hello World 123\"))))"

        it "parses an @base declaration containing a hash fragment" $ do
            let tokens = TurtleLexer.alexScanTokens "@base <http://example.org/ns#> ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "Terminal (Base \"http://example.org/ns#\")"

        it "parses an @prefix declaration containing a hash fragment" $ do
            let tokens = TurtleLexer.alexScanTokens "@prefix x: <http://example.org/x#> ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "Terminal (Prefix \"x\" \"http://example.org/x#\")"

        it "parses a comma separated list of string literals" $ do
            let tokens = TurtleLexer.alexScanTokens "<x> <y> \"one\" , \"two\" ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "Terminal (Triple (Postfix \"x\") (SingularPredObj (Postfix \"y\") (MultipleObj (LitStr \"one\") (SingularObj (LitStr \"two\")))))"

        it "parses a comma separated list with mixed object types" $ do
            let tokens = TurtleLexer.alexScanTokens "<x> <y> 1 , \"two\" , <three> ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "Terminal (Triple (Postfix \"x\") (SingularPredObj (Postfix \"y\") (MultipleObj (LitInt 1) (MultipleObj (LitStr \"two\") (SingularObj (Uri (Postfix \"three\")))))))"

        it "parses an @base declaration followed by a triple" $ do
            let tokens = TurtleLexer.alexScanTokens "@base <http://ex.org/> . <subj> <pred> <obj> ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "NonTerminal (Base \"http://ex.org/\") (Terminal (Triple (Postfix \"subj\") (SingularPredObj (Postfix \"pred\") (SingularObj (Uri (Postfix \"obj\"))))))"

        it "parses an @prefix declaration followed by a triple" $ do
            let tokens = TurtleLexer.alexScanTokens "@prefix ex: <http://ex.org/> . ex:a ex:b ex:c ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "NonTerminal (Prefix \"ex\" \"http://ex.org/\") (Terminal (Triple (Short \"ex\" \"a\") (SingularPredObj (Short \"ex\" \"b\") (SingularObj (Uri (Short \"ex\" \"c\"))))))"

        it "parses two consecutive triples" $ do
            let tokens = TurtleLexer.alexScanTokens "<x> <y> 1 . <a> <b> 2 ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "NonTerminal (Triple (Postfix \"x\") (SingularPredObj (Postfix \"y\") (SingularObj (LitInt 1)))) (Terminal (Triple (Postfix \"a\") (SingularPredObj (Postfix \"b\") (SingularObj (LitInt 2)))))"

        it "parses two identical triples across statements" $ do
            let tokens = TurtleLexer.alexScanTokens "<x> <y> 1 . <x> <y> 1 ."
            show (TurtleParser.parseCalc tokens) `shouldBe` "NonTerminal (Triple (Postfix \"x\") (SingularPredObj (Postfix \"y\") (SingularObj (LitInt 1)))) (Terminal (Triple (Postfix \"x\") (SingularPredObj (Postfix \"y\") (SingularObj (LitInt 1)))))"



    describe "RDF Object Sorting" $ do
        it "sorts strings before integers" $ do
            (LitStr "zzz" < LitInt 1) `shouldBe` True
            (LitStr "10" < LitInt 10) `shouldBe` True

        it "sorts integers before URIs" $ do
            (LitInt 100 < Uri (FullURL "http://example.org")) `shouldBe` True

        it "sorts strings before URIs" $ do
            (LitStr "abc" < Uri (FullURL "http://example.org")) `shouldBe` True

        it "sorts entire triples by subject first, overriding object values" $ do
            let uriA = FullURL "http://a.com"
            let uriB = FullURL "http://b.com"
            let p = FullURL "http://pred.com"
            ((uriA, p, LitInt 100) < (uriB, p, LitInt 1)) `shouldBe` True

        it "sorts entire triples by predicate second, if subjects are equal" $ do
            let s = FullURL "http://subj.com"
            let predA = FullURL "http://predA.com"
            let predB = FullURL "http://predB.com"
            ((s, predA, LitInt 100) < (s, predB, LitInt 1)) `shouldBe` True

        it "sorts entire triples by object third, if subjects and predicates are equal" $ do
            let s = FullURL "http://subj.com"
            let p = FullURL "http://pred.com"
            ((s, p, LitInt 1) < (s, p, LitInt 100)) `shouldBe` True



    describe "Turtle Evaluator" $ do
        let parseTTL = TurtleParser.parseCalc . TurtleLexer.alexScanTokens

        it "fully expands @base URIs correctly mapped to Postfix" $ do
            let ast = parseTTL "@base <http://example.org/> . <subj> <pred> 1 ."
            let triples = TurtleEval.turtleIntr ast
            triples `shouldBe` [(FullURL "http://example.org/subj", FullURL "http://example.org/pred", LitInt 1)]

        it "fully expands @prefix URIs into Short aliases" $ do
            let ast = parseTTL "@prefix ex: <http://example.org/> . ex:a ex:b \"test\" ."
            let triples = TurtleEval.turtleIntr ast
            triples `shouldBe` [(FullURL "http://example.org/a", FullURL "http://example.org/b", LitStr "test")]

        it "handles multiple predicates using semicolons (creates multiple triples)" $ do
            let ast = parseTTL "@base <http://ex.org/> . <s> <p1> 1 ; <p2> 2 ."
            let triples = TurtleEval.turtleIntr ast
            triples `shouldBe` [ (FullURL "http://ex.org/s", FullURL "http://ex.org/p1", LitInt 1), (FullURL "http://ex.org/s", FullURL "http://ex.org/p2", LitInt 2) ]

        it "handles multiple objects using commas (creates multiple triples)" $ do
            let ast = parseTTL "@base <http://ex.org/> . <s> <p> 1 , 2 ."
            let triples = TurtleEval.turtleIntr ast
            triples `shouldBe` [ (FullURL "http://ex.org/s", FullURL "http://ex.org/p", LitInt 1), (FullURL "http://ex.org/s", FullURL "http://ex.org/p", LitInt 2) ]


    describe "RQL Evaluator" $ do
        let s1 = FullURL "http://ex.org/s1"
        let p1 = FullURL "http://ex.org/p1"
        let t1 = (s1, p1, LitInt 10)
        let t2 = (s1, p1, LitInt 20)
        let t3 = (FullURL "http://ex.org/s2", p1, LitStr "foo")

        it "ObjectComp handles >= to filter integers successfully" $ do
            let p = ObjectComp Gte 15
            matchPattern p t1 `shouldBe` False
            matchPattern p t2 `shouldBe` True
            matchPattern p t3 `shouldBe` False

        it "Conjunction filters logically (AND)" $ do
            let p = Conjunction (PredicateEq p1) (ObjectComp Lt 15)
            matchPattern p t1 `shouldBe` True
            matchPattern p t2 `shouldBe` False

        it "MAX correctly finds maximum integer within grouping" $ do
            let res = applyAggregate GroupSubject Max [t1, t2]
            res `shouldBe` [(s1, p1, LitInt 20)]

        it "Joins by Subject correctly and outputs matched Triple form" $ do
            let joinMatch = JoinMatch S S S1 P1 O2
            let g1 = [(s1, p1, LitInt 1)]
            let g2 = [(s1, FullURL "http://ex.org/p2", LitInt 2)]
            let res = applyJoin joinMatch g1 g2
            res `shouldBe` [(s1, p1, LitInt 2)]



    -- Require .ttl files for testing

    -- describe "Task Integration" $ do

    --     it "Output Validation" $ do
    --         contents <- readFile "t1.rql"
    --         res <- rqlMain contents
    --         res `shouldBe` "<http://example.org/nmg> <http://ont.example.org/name> \"Nick Gibbins\" .\n" ++
    --                        "<http://example.org/nmg> <http://ont.example.org/teaches> <http://example.org/comp2312> .\n" ++
    --                        "<http://example.org/nmg> <http://ont.example.org/teaches> <http://example.org/comp2322> .\n" ++
    --                        "<http://example.org/nmg> <http://ont.example.org/teaches> <http://example.org/comp2332> .\n" ++
    --                        "<http://example.org/nmg> <http://ont.example.org/worksFor> <http://example.org/uos> ."
        
    --     it "Task 1: Graph Union combines files and removes duplicates" $ do
    --         contents <- readFile "t1.rql"
    --         res <- rqlMain contents
    --         res `shouldContain` "<http://ont.example.org/name> \"Nick Gibbins\" ."
    --         length (lines res) `shouldSatisfy` (> 0)

    --     it "Task 2: Pattern Matching filters and outputs matching integer subset" $ do
    --         contents <- readFile "t2.rql"
    --         res <- rqlMain contents
    --         res `shouldContain` "21 ."
    --         res `shouldContain` "23 ."
    --         res `shouldNotContain` "20 ."

    --     it "Task 3: Further Pattern Matching with UNION combinations" $ do
    --         contents <- readFile "t3.rql"
    --         res <- rqlMain contents
    --         res `shouldContain` "<http://example.org/uos>"
            
    --     it "Task 4: Aggregates produces the absolute MAX values" $ do
    --         contents <- readFile "t4.rql"
    --         res <- rqlMain contents
    --         res `shouldContain` "7 ."
    --         res `shouldContain` "9 ."
    --         res `shouldNotContain` "2 ."
    --         res `shouldNotContain` "5 ."

    --     it "Task 5: Graph Edits executes JOIN properly for subclass derivation" $ do
    --         contents <- readFile "t5.rql"
    --         res <- rqlMain contents
    --         res `shouldContain` "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/Person> ."



    describe "Robustness & Edge-Case Evaluation" $ do
        let uriA = FullURL "http://ex.org/A"
        let uriB = FullURL "http://ex.org/B"
        let uriC = FullURL "http://ex.org/C"
        let pred1 = FullURL "http://ex.org/p1"
        let g1 = [(uriA, pred1, LitInt 10), (uriB, pred1, LitInt 20)]
        let g2 = [(uriB, pred1, LitInt 20), (uriC, pred1, LitInt 30)]

        it "INTERSECTION correctly identifies overlaps" $ do
            let res = intersect g1 g2
            res `shouldBe` [(uriB, pred1, LitInt 20)]

        it "DIFFERENCE correctly subtracts right graph from left" $ do
            let res1 = g1 \\ g2
            res1 `shouldBe` [(uriA, pred1, LitInt 10)]
            let res2 = g2 \\ g1
            res2 `shouldBe` [(uriC, pred1, LitInt 30)]

        it "SUM calculates total correctly" $ do
            let res = applyAggregate None Sum g1
            res `shouldBe` [(uriA, pred1, LitInt 30)]

        it "MIN finds the absolute lowest integer" $ do
            let res = applyAggregate None Min g1
            res `shouldBe` [(uriA, pred1, LitInt 10)]

        it "COUNT tallies the number of triples" $ do
            let res = applyAggregate None Count g1
            res `shouldBe` [(uriA, pred1, LitInt 2)]

        it "AVG calculates integer division average safely" $ do
            let g3 = [(uriA, pred1, LitInt 10), (uriA, pred1, LitInt 20), (uriA, pred1, LitInt 30)]
            let res = applyAggregate None Avg g3
            res `shouldBe` [(uriA, pred1, LitInt 20)]

        it "AVG does not crash on empty lists" $ do
            let res = applyAggregate None Avg []
            res `shouldBe` []

        it "LOuterJoin preserves unmatched left-side triples" $ do
            let joinMatch = JoinMatch S S S1 P1 O2
            let leftGraph = [(uriA, pred1, LitInt 1), (uriB, pred1, LitInt 2)]
            let rightGraph = [(uriA, FullURL "http://ex.org/p2", LitInt 99)]
            let res = applyLOuterJoin joinMatch leftGraph rightGraph
            res `shouldContain` [(uriA, pred1, LitInt 99)]
            res `shouldContain` [(uriB, pred1, LitInt 2)]

        it "Terminates successfully on a Cyclic Graph to prevent infinite loops" $ do
            let cycleGraph = [ (uriA, pred1, Uri uriB), (uriB, pred1, Uri uriA) ]
            let p = PredicateEq pred1
            let res = applyTransitive p cycleGraph
            res `shouldContain` [(uriA, pred1, Uri uriA)]
            res `shouldContain` [(uriB, pred1, Uri uriB)]
            length res `shouldBe` 4

        it "LIMIT restricts the size of the output graph" $ do
            let g3 = [(uriA, pred1, LitInt 1), (uriA, pred1, LitInt 2), (uriA, pred1, LitInt 3)]
            let res = applyLimit 2 g3
            length res `shouldBe` 2
            res `shouldBe` [(uriA, pred1, LitInt 1), (uriA, pred1, LitInt 2)]

        it "OFFSET skips triples in the graph" $ do
            let g3 = [(uriA, pred1, LitInt 1), (uriA, pred1, LitInt 2), (uriA, pred1, LitInt 3)]
            let res = applyOffset 1 g3
            length res `shouldBe` 2
            res `shouldBe` [(uriA, pred1, LitInt 2), (uriA, pred1, LitInt 3)]

        it "RENAME correctly swaps URI/Object positions" $ do
            let tUri = (uriA, pred1, Uri uriB)
            let resUri = applyRename FromObject FromPredicate FromSubject tUri
            resUri `shouldBe` (uriB, pred1, Uri uriA)

        it "Disjunction (OR) and Negation (NOT) evaluate correctly" $ do
            let pOr = Disjunction (ObjectEq (LitInt 10)) (ObjectEq (LitInt 20))
            matchPattern pOr (uriA, pred1, LitInt 10) `shouldBe` True
            matchPattern pOr (uriA, pred1, LitInt 30) `shouldBe` False
            let pNot = Negation (PredicateEq pred1)
            matchPattern pNot (uriA, pred1, LitInt 10) `shouldBe` False
            matchPattern pNot (uriA, FullURL "http://ex.org/p2", LitInt 10) `shouldBe` True
