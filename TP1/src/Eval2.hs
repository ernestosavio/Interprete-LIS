module Eval2
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
initState =  M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor var s = case M.lookup var s of
                  Nothing   -> Left UndefVar
                  (Just v)  -> Right v

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update var val s = M.update f var s
  where
  f :: Int -> Maybe Int
  f _ = (Just val)

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip s = Right (Skip :!: s) 
stepComm (Let var e) s = case (evalExp e s) of
                          Right (n :!: s') -> let 
                                                s'' = M.insert var n s'
                                              in 
                                                  Right (Skip :!: s'')
                          Left err         -> Left err

stepComm (Seq Skip c) s = Right (c :!: s)
stepComm (Seq c0 c1) s = case (stepComm c0 s) of
                            Right (c0' :!: s') -> Right ((Seq c0' c1) :!: s')
                            x                  -> x
                            
stepComm (IfThenElse p c0 c1) s = case (evalExp p s) of
                                      Right (b :!: s') -> if b then (stepComm c0 s') else (stepComm c1 s')
                                      Left err         -> Left err

stepComm r@(RepeatUntil c p) s = let 
                                     c' = (IfThenElse p Skip r)
                                 in 
                                    stepComm (Seq c c') s
                                          



-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const n) s = Right (n :!: s)
evalExp (Var var) s = case (lookfor var s) of
                          Right n -> Right (n :!: s)
                          Left x  -> Left x
evalExp (VarInc var) s = case (lookfor var s) of
                          Right n -> let s' = update var (n + 1) s in Right ((n + 1) :!: s') 
                          Left x  -> Left x
                            
evalExp (UMinus e) s = case (evalExp e s) of 
                          Right (n :!: s') -> Right ((-n) :!: s')
                          Left err         -> Left err

                          
evalExp (Plus e0 e1) s = case (evalExp e0 s) of 
                          Right (n0 :!: s0) -> case (evalExp e1 s0) of 
                                                  Right (n1 :!: s1) -> Right ((n0 + n1) :!: s1)
                                                  Left err          -> Left err
                          Left err          -> Left err

evalExp (Minus e0 e1) s = case (evalExp e0 s) of 
                          Right (n0 :!: s0) -> case (evalExp e1 s0) of 
                                                  Right (n1 :!: s1) -> Right ((n0 - n1) :!: s1)
                                                  Left err1         -> Left err1
                          Left err0         -> Left err0

evalExp (Times e0 e1) s = case (evalExp e0 s) of 
                          Right (n0 :!: s0) -> case (evalExp e1 s0) of 
                                                  Right (n1 :!: s1) -> Right ((n0 * n1) :!: s1)
                                                  Left err1         -> Left err1
                          Left err0         -> Left err0
                          
evalExp (Div e0 e1) s = case (evalExp e0 s) of 
                          Right (n0 :!: s0) -> case (evalExp e1 s0) of 
                                                  Right (0 :!: _)  -> Left DivByZero
                                                  Right (n1 :!: s1) -> Right ((n0 `div` n1) :!: s1)
                                                  Left err1         -> Left err1
                          Left err0         -> Left err0

evalExp BTrue s  = Right (True :!: s)
evalExp BFalse s = Right (False :!: s)
evalExp (And p0 p1) s = case (evalExp p0 s) of 
                          Right (b0 :!: s0) -> case (evalExp p1 s0) of 
                                                  Right (b1 :!: s1) -> Right ((b0 && b1) :!: s1)
                                                  Left err1         -> Left err1
                          Left err0         -> Left err0
                            
evalExp (Or p0 p1) s = case (evalExp p0 s) of 
                          Right (b0 :!: s0) -> case (evalExp p1 s0) of 
                                                  Right (b1 :!: s1) -> Right ((b0 || b1) :!: s1)
                                                  Left err1         -> Left err1
                          Left err0         -> Left err0

evalExp (Not p) s = case (evalExp p s) of
                      Right (b :!: s') -> Right ((not b) :!: s')
                      Left err -> Left err

evalExp (Lt e0 e1) s = case (evalExp e0 s) of 
                          Right (n0 :!: s0) -> case (evalExp e1 s0) of 
                                                  Right (n1 :!: s1) -> Right ((n0 < n1) :!: s1)
                                                  Left err1         -> Left err1
                          Left err0         -> Left err0
                            
evalExp (Gt e0 e1) s = case (evalExp e0 s) of 
                          Right (n0 :!: s0) -> case (evalExp e1 s0) of 
                                                  Right (n1 :!: s1) -> Right ((n0 > n1) :!: s1)
                                                  Left err1         -> Left err1
                          Left err0         -> Left err0

evalExp (Eq e0 e1) s = case (evalExp e0 s) of 
                          Right (n0 :!: s0) -> case (evalExp e1 s0) of 
                                                  Right (n1 :!: s1) -> Right ((n0 == n1) :!: s1)
                                                  Left err1         -> Left err1
                          Left err0         -> Left err0

evalExp (NEq e0 e1) s = case (evalExp e0 s) of 
                          Right (n0 :!: s0) -> case (evalExp e1 s0) of 
                                                  Right (n1 :!: s1) -> Right ((not (n0 == n1)) :!: s1)
                                                  Left err1         -> Left err1
                          Left err0         -> Left err0
                            

