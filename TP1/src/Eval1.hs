module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado vacío
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor var s = s M.! var

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update var val s = M.update f var s
  where
  f :: Int -> Maybe Int
  f _ = (Just val)

-- Evalúa un programa en el estado vacío
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip s = (Skip :!: s) 
stepComm (Let var e) s = let 
                            (n :!: s') = evalExp e s
                            s'' = M.insert var n s'
                         in 
                            (Skip :!: s'')
stepComm (Seq Skip c) s = (c :!: s)
stepComm (Seq c0 c1) s = let
                             (c0' :!: s') = stepComm c0 s
                         in
                            ((Seq c0' c1) :!: s')
stepComm (IfThenElse p c0 c1) s = let
                                      (b :!: s') = evalExp p s
                                  in 
                                      if b 
                                        then (stepComm c0 s')
                                        else (stepComm c1 s')
stepComm r@(RepeatUntil c p) s = let 
                                     c' = (IfThenElse p Skip r)
                                 in 
                                    stepComm (Seq c c') s
                                          

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
evalExp (Const n) s = (n :!: s)
evalExp (Var var) s = let n = lookfor var s in (n :!: s)
evalExp (VarInc var) s = let 
                            n = lookfor var s 
                            s' = update var (n + 1) s
                         in 
                            ((n + 1) :!: s') 
evalExp (UMinus e) s = let 
                          (n :!: s') = evalExp e s
                       in
                          ((-n) :!: s')
evalExp (Plus e0 e1) s = let
                            (n0 :!: s0) = evalExp e0 s
                            (n1 :!: s1) = evalExp e1 s0
                         in
                            ((n0 + n1) :!: s1)
evalExp (Minus e0 e1) s = let
                            (n0 :!: s0) = evalExp e0 s
                            (n1 :!: s1) = evalExp e1 s0
                         in
                            ((n0 - n1) :!: s1)
evalExp (Times e0 e1) s = let
                            (n0 :!: s0) = evalExp e0 s
                            (n1 :!: s1) = evalExp e1 s0
                         in
                            ((n0 * n1) :!: s1)
evalExp (Div e0 e1) s = let
                            (n0 :!: s0) = evalExp e0 s
                            (n1 :!: s1) = evalExp e1 s0
                         in
                            ((n0 `div` n1) :!: s1)
evalExp BTrue s = (True :!: s)
evalExp BFalse s = (False :!: s)
evalExp (And p0 p1) s = let
                            (b0 :!: s0) = evalExp p0 s
                            (b1 :!: s1) = evalExp p1 s0
                        in
                            ((b0 && b1) :!: s1)
evalExp (Or p0 p1) s = let
                            (b0 :!: s0) = evalExp p0 s
                            (b1 :!: s1) = evalExp p1 s0
                        in
                            ((b0 || b1) :!: s1)
evalExp (Not p) s = let (b :!: s') = evalExp p s in ((not b) :!: s')
evalExp (Lt e0 e1) s = let
                            (n0 :!: s0) = evalExp e0 s
                            (n1 :!: s1) = evalExp e1 s0
                        in
                            ((n0 < n1) :!: s1)
evalExp (Gt e0 e1) s = let
                            (n0 :!: s0) = evalExp e0 s
                            (n1 :!: s1) = evalExp e1 s0
                        in
                            ((n0 > n1) :!: s1)
evalExp (Eq e0 e1) s = let
                            (n0 :!: s0) = evalExp e0 s
                            (n1 :!: s1) = evalExp e1 s0
                        in
                            ((n0 == n1) :!: s1)
evalExp (NEq e0 e1) s = let
                            (n0 :!: s0) = evalExp e0 s
                            (n1 :!: s1) = evalExp e1 s0
                        in
                            ((not (n0 == n1)) :!: s1)
