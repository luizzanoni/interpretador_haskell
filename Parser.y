{
module Parser where 

import Lexer

}

%name parser
%tokentype { Token }
%error { parseError }

%token
  true          { TokenTrue }
  false         { TokenFalse }
  num           { TokenNum $$ }
  '+'           { TokenAdd }
  '-'           { TokenSub }
  and           { TokenAnd }
  "=="          { TokenEq }
  if            { TokenIf }
  then          { TokenThen }
  else          { TokenElse }
  '('           { TokenLParen }
  ')'           { TokenRParen }
  ','           { TokenComma }
  '*'           { TokenMulti }
  var           { TokenVar $$ }

%nonassoc if then else 
%left "=="
%left '+' and
%left '*'
%left '-'
%%

Exp : true                        { BTrue }
    | false                       { BFalse }
    | num                         { Num $1 }
    | var                         { Var $1 }  -- Adicionando regra para variáveis
    | Exp '+' Exp                 { Add $1 $3 }
    | Exp and Exp                 { And $1 $3 }
    | Exp "==" Exp                { Eq $1 $3 }
    | Exp '-' Exp                 { Sub $1 $3 }
    | if Exp then Exp else Exp    { If $2 $4 $6 }
    | '(' TupleExps ')'           { Tuple $2 }  -- Modificação para tuplas
    | Exp '*' Exp                 { Multi $1 $3 }

TupleExps : Exp                   { [$1] }
          | Exp ',' TupleExps     { $1 : $3 }

{
parseError :: [Token] -> a 
parseError ts = error "Syntax error: sequência de instruções inválidas."
}
