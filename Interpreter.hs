module Interpreter where

import Lexer 

-- Função para verificar se uma expressão é um valor
isValue :: Expr -> Bool
isValue BTrue = True 
isValue BFalse = True 
isValue (Num _) = True 
isValue (Lam _ _ _) = True
isValue (Tuple _) = True  -- Reconhecendo tuplas como valores
isValue _ = False 

-- Função para acessar elementos de uma tupla
accessTuple :: Expr -> Int -> Maybe Expr
accessTuple (Tuple xs) n 
    | n >= 0 && n < length xs = Just (xs !! n)
    | otherwise = Nothing
accessTuple _ _ = Nothing

subst :: String -> Expr -> Expr -> Expr
subst x n b@(Var v) = if v == x then 
                        n 
                      else 
                        b 
subst x n (Add e1 e2) = Add (subst x n e1) (subst x n e2)
subst x n (Sub e1 e2) = Sub (subst x n e1) (subst x n e2)
subst x n (And e1 e2) = And (subst x n e1) (subst x n e2)
subst x n (Eq e1 e2) = Eq (subst x n e1) (subst x n e2)
subst x n (If e e1 e2) = If (subst x n e) (subst x n e1) (subst x n e2)
subst x n (Lam v t b) = Lam v t (subst x n b)
subst x n (App e1 e2) = App (subst x n e1) (subst x n e2)
subst x n (Multi e1 e2) = Multi (subst x n e1) (subst x n e2)
subst x n (Tuple es) = Tuple (map (subst x n) es)
subst _ _ e = e

step :: Expr -> Expr 
step (Multi (Num n1) (Num n2)) = Num (n1 * n2)  -- Multiplicação
step (Multi (Num nv) e2) = let e2' = step e2 
                           in Multi (Num nv) e2' 
step (Multi e1 e2) = Multi (step e1) e2

step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Sub (Num n1) (Num n2)) = Num (n1 - n2)  -- Passo para a operação de subtração
step (Add (Num nv) e2) = Add (Num nv) (step e2) 
step (Sub (Num nv) e2) = Sub (Num nv) (step e2) 
step (Add e1 e2) = Add (step e1) e2 
step (Sub e1 e2) = Sub (step e1) e2
step (And BFalse e) = BFalse 
step (And BTrue e) = e 
step (And e1 e2) = And (step e1) e2 

step (Eq e1 e2) | isValue e1 && isValue e2 = if (e1 == e2) then 
                                               BTrue 
                                             else 
                                               BFalse
                | isValue e1 = Eq e1 (step e2)
                | otherwise = Eq (step e1) e2
step (If BTrue e1 e2) = e1 
step (If BFalse e1 e2) = e2 
step (If e e1 e2) = If (step e) e1 e2 
step (App (Lam v t b) e) | isValue e = subst v e b
                         | otherwise = (App (Lam v t b) (step e))
step (App e1 e2) = App (step e1) e2
step (Tuple es) = Tuple (map step es)  -- Passo para tuplas com múltiplas posições

eval :: Expr -> Expr 
eval e | isValue e = e 
       | otherwise = eval (step e) 
