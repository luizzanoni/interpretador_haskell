module Parser where

import Lexer

parser :: [Token] -> Expr
parser tokens = case tokens of
    (TokenNum n : rest) -> (Num n, rest)
    (TokenTrue : rest) -> (BTrue, rest)
    (TokenFalse : rest) -> (BFalse, rest)
    (TokenListOpen : rest) -> parseList rest
    (TokenVar v : rest) -> (Var v, rest)
    ...

parseList :: [Token] -> (Expr, [Token])
parseList tokens = case tokens of
    (TokenListClose : rest) -> (List [], rest) -- Lista vazia
    _ -> let (head, rest1) = parser tokens
             (tail, rest2) = parseListTail rest1
         in (Cons head tail, rest2)

parseListTail :: [Token] -> (Expr, [Token])
parseListTail tokens = case tokens of
    (TokenCons : rest) -> parseList rest
    (TokenListClose : rest) -> (List [], rest)
    _ -> error "Erro ao parsear lista"
