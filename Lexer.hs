module Lexer where 

import Data.Char


-- Definição do tipo de expressão (Expr)
data Expr = Num Int        -- Constantes numéricas
          | BTrue          -- Valor booleano True
          | BFalse         -- Valor booleano False
          | Add Expr Expr  -- Adição
          | Sub Expr Expr  -- Subtração
          | And Expr Expr  -- Operação AND
          | Eq Expr Expr   -- Comparação de igualdade
          | If Expr Expr Expr -- Condicional If
          | Var String     -- Variáveis
          | Lam String Ty Expr -- Função Lambda
          | App Expr Expr  -- Aplicação de função
          | Tuple Expr Expr -- Tuplas
          | Multi Expr Expr
          
          deriving (Show, Eq)

-- Definição de tipos adicionais, como o tipo Ty
data Ty = TNum 
        | TBool 
        | TFun Ty Ty 
        | TTuple Ty Ty
        deriving (Show, Eq)

data Token = TokenTrue
           | TokenFalse 
           | TokenNum Int 
           | TokenAdd 
           | TokenSub          -- Adicionando o token de subtração
           | TokenAnd 
           | TokenEq
           | TokenIf
           | TokenThen
           | TokenElse 
           | TokenVar String
           | TokenLam 
           | TokenArrow 
           | TokenLParen       -- Parênteses para tuplas
           | TokenRParen
           | TokenComma  
           | TokenMulti      -- Vírgula para tuplas
           deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer ('+':cs) = TokenAdd : lexer cs
lexer ('-':cs) = TokenSub : lexer cs
lexer ('\\':cs) = TokenLam : lexer cs
lexer ('=':'=':cs) = TokenEq : lexer cs
lexer ('(':cs) = TokenLParen : lexer cs  -- Para o parêntese esquerdo
lexer (')':cs) = TokenRParen : lexer cs  -- Para o parêntese direito
lexer (',':cs) = TokenComma : lexer cs   -- Para a vírgula
lexer ('*':cs) = TokenMulti : lexer cs
lexer (c:cs) 
    | isSpace c = lexer cs
    | isAlpha c = lexerKW (c:cs)
    | isDigit c = lexerNum (c:cs)
   

lexerNum :: String -> [Token]
lexerNum cs = case span isDigit cs of 
    (num, rest) -> TokenNum (read num) : lexer rest

lexerKW :: String -> [Token]
lexerKW cs = case span isAlpha cs of 
    ("true", rest) -> TokenTrue : lexer rest
    ("false", rest) -> TokenFalse : lexer rest
    ("and", rest) -> TokenAnd : lexer rest
    ("if", rest) -> TokenIf : lexer rest
    ("then", rest) -> TokenThen : lexer rest
    ("else", rest) -> TokenElse : lexer rest
    -- (var, rest) -> TokenVar var : lexer rest
