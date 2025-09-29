module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados 
type State = (M.Map Variable Int, String)

-- Estado vacío
-- Completar la definición
initState :: State
initState = (M.empty, "")

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor var (s,_) = case M.lookup var s of
                  Nothing   -> Left UndefVar
                  (Just v)  -> Right v

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update var val (s,t) = addTrace ("Let" ++ (var ++ (show val))) (M.update f var s, t)
  where
  f :: Int -> Maybe Int
  f _ = (Just val)

-- Agrega una traza dada al estado
-- Completar la definición
addTrace :: String -> State -> State
addTrace trace (s, t) = (s, t ++ trace) 

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
                          Right (n :!: (s',t)) -> let 
                                                    s'' = M.insert var n s'
                                                    s''' = addTrace ("Let" ++ (var ++ (show n))) (s'',t)
                                                  in 
                                                      Right (Skip :!: s''')
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
evalExp = undefined
