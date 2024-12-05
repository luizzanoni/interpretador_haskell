module Main where 

import Lexer 
import Parser
import Interpreter
import TypeChecker

main = do
    let tupleExpr = Tuple [Var "teste1", Var "teste2", Num 1]
    print $ accessTuple tupleExpr 0 -- Deve retornar Just (Var "teste1")
    print $ accessTuple tupleExpr 1 -- Deve retornar Just (Var "teste2")
    print $ accessTuple tupleExpr 2 -- Deve retornar Just (Num 1)
    print $ accessTuple tupleExpr 3 -- Deve retornar Nothing (fora dos limites)
    
    -- Executando o interpretador completo
    getContents >>= print . eval . typecheck . parser . lexer
