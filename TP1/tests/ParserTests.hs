module ParserTests where

import Test.HUnit

import AST
import Parser
import Text.Parsec (ParseError)

-- | Tests cases templates

okTest :: FilePath -> String -> String -> Either ParseError Comm -> Test
okTest filePath cont msg expected =
  TestCase $ assertEqual msg expected (parseComm filePath cont)


-- | Tests cases definition

testSkip :: FilePath -> String -> Test
testSkip filePath cont = okTest filePath cont "Error on skip" (Right Skip)

testSqrt :: FilePath -> String -> Test
testSqrt filePath cont = okTest filePath cont msg (Right prog)
  where
    msg = "Error on parser for sqrt program"
    prog = Seq (Seq (Let "n" (Const 25)) 
                    (Let "i" (UMinus (Const 1)))) 
               (RepeatUntil (Seq (Let "i" (Plus (Var "i") (Const 1))) (Let "t" (Times (Var "i") (Var "i")))) 
                            (Or (Gt (Var "t") (Var "n")) (Eq (Var "t") (Var "n"))))

testSimpleAssign :: FilePath -> String -> Test
testSimpleAssign filePath cont = okTest filePath cont msg (Right prog)
  where
    msg = "Error on simple assignment"
    prog = Let "x" (Const 42)

testArithmetic :: FilePath -> String -> Test
testArithmetic filePath cont = okTest filePath cont msg (Right prog)
  where
    msg = "Error on arithmetic expression"
    prog = Let "result" (Minus (Times (Plus (Const 5) (Const 3)) (Const 2)) (Const 1))

testNegative :: FilePath -> String -> Test
testNegative filePath cont = okTest filePath cont msg (Right prog)
  where
    msg = "Error on negative numbers"
    prog = Seq (Let "x" (UMinus (Const 10))) (Let "y" (UMinus (Var "x")))

testIncrement :: FilePath -> String -> Test
testIncrement filePath cont = okTest filePath cont msg (Right prog)
  where
    msg = "Error on increment operator"
    prog = Seq (Let "x" (Const 5)) (Let "y" (VarInc "x"))

testIfThen :: FilePath -> String -> Test
testIfThen filePath cont = okTest filePath cont msg (Right prog)
  where
    msg = "Error on if-then"
    prog = Seq (Let "x" (Const 10)) 
               (IfThen (Gt (Var "x") (Const 5)) (Let "y" (Times (Var "x") (Const 2))))

testIfThenElse :: FilePath -> String -> Test
testIfThenElse filePath cont = okTest filePath cont msg (Right prog)
  where
    msg = "Error on if-then-else"
    prog = Seq (Let "x" (Const 3))
               (IfThenElse (Gt (Var "x") (Const 5)) 
                          (Let "y" (Const 1))
                          (Let "y" (Const 2)))

testBooleanOps :: FilePath -> String -> Test
testBooleanOps filePath cont = okTest filePath cont msg (Right prog)
  where
    msg = "Error on boolean operations"
    prog = Seq (Let "x" (Const 5))
               (IfThenElse (Or (And (Gt (Var "x") (Const 3)) (Lt (Var "x") (Const 10))) 
                              (Eq (Var "x") (Const 0)))
                          (Let "result" (Const 1))
                          (Let "result" (Const 0)))

testNestedIf :: FilePath -> String -> Test
testNestedIf filePath cont = okTest filePath cont msg (Right prog)
  where
    msg = "Error on nested if"
    prog = Seq (Let "x" (Const 7))
               (IfThenElse (Gt (Var "x") (Const 5))
                          (IfThenElse (Lt (Var "x") (Const 10))
                                     (Let "result" (Const 1))
                                     (Let "result" (Const 2)))
                          (Let "result" (Const 0)))

testRepeatSimple :: FilePath -> String -> Test
testRepeatSimple filePath cont = okTest filePath cont msg (Right prog)
  where
    msg = "Error on simple repeat"
    prog = Seq (Let "i" (Const 0))
               (RepeatUntil (Let "i" (Plus (Var "i") (Const 1)))
                           (Gt (Var "i") (Const 4)))

testDivision :: FilePath -> String -> Test
testDivision filePath cont = okTest filePath cont msg (Right prog)
  where
    msg = "Error on division"
    prog = Seq (Seq (Let "x" (Const 15)) (Let "y" (Const 3)))
               (Let "result" (Div (Var "y") (Var "x")))

testComparison :: FilePath -> String -> Test
testComparison filePath cont = okTest filePath cont msg (Right prog)
  where
    msg = "Error on comparison operators"
    prog = Seq (Seq (Seq (Seq (Seq (Let "x" (Const 10)) 
                              (Let "y" (Const 20)))
                          (IfThen (Lt (Var "x") (Var "y")) (Let "less" (Const 1))))
                     (IfThen (Gt (Var "x") (Var "y")) (Let "greater" (Const 1))))
               	(IfThen (Eq (Var "x") (Var "y")) (Let "equal" (Const 1))))
                    (IfThen (NEq (Var "x") (Var "y")) (Let "not_equal" (Const 1)))

testCaseSimple :: FilePath -> String -> Test
testCaseSimple filePath cont = okTest filePath cont msg (Right prog)
  where
    msg = "Error on simple case"
    prog = Seq (Let "x" (Const 5))
               (Case [(Gt (Var "x") (Const 10), Seq (Let "result" (Const 1)) (Let "resultt" (Const 3))),
                      (Gt (Var "x") (Const 0), Let "result" (Const 2))])


-- | Tests cases

tests :: [(FilePath -> String -> Test, FilePath)]
tests =
    [
      (testSkip, "ejemplos/skip.lis")
    , (testSqrt, "ejemplos/sqrt.lis")
    , (testSimpleAssign, "ejemplos/simple_assign.lis")
    , (testArithmetic, "ejemplos/arithmetic.lis")
    , (testNegative, "ejemplos/negative.lis")
    , (testIncrement, "ejemplos/increment.lis")
    , (testIfThen, "ejemplos/if_then.lis")
    , (testIfThenElse, "ejemplos/if_then_else.lis")
    , (testBooleanOps, "ejemplos/boolean_ops.lis")
    , (testNestedIf, "ejemplos/nested_if.lis")
    , (testRepeatSimple, "ejemplos/repeat_simple.lis")
    , (testDivision, "ejemplos/division.lis")
    , (testComparison, "ejemplos/comparison.lis")
    , (testCaseSimple, "ejemplos/case_simple.lis")
    ]


-- | Run tests

parserTests :: IO Counts
parserTests = do
    ts <- mapM buildTest tests
    runTestTT $ TestList ts

buildTest :: (FilePath -> String -> Test, FilePath) -> IO Test
buildTest (testSpec, fp) = do
    content <- readFile fp
    return (testSpec fp content)
