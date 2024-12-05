module Interpreter where

import Lexer

eval :: Expr -> Expr
eval (Num n) = Num n
eval BTrue = BTrue
eval BFalse = BFalse
eval (Add e1 e2) = case (eval e1, eval e2) of
                     (Num n1, Num n2) -> Num (n1 + n2)
                     _ -> error "Erro de tipo em Add"
eval (And e1 e2) = case (eval e1, eval e2) of
                     (BTrue, BTrue) -> BTrue
                     (BTrue, BFalse) -> BFalse
                     (BFalse, _) -> BFalse
                     _ -> error "Erro de tipo em And"
eval (Eq e1 e2) = case (eval e1, eval e2) of
                    (Num n1, Num n2) -> if n1 == n2 then BTrue else BFalse
                    _ -> error "Erro de tipo em Eq"
eval (If e e1 e2) = case eval e of
                      BTrue -> eval e1
                      BFalse -> eval e2
                      _ -> error "Erro de tipo em If"
eval (List xs) = List (map eval xs)
eval (Cons h t) = case eval t of
                    List ts -> List (eval h : ts)
                    _ -> error "Cauda de uma lista deve ser uma lista"
eval e = e
