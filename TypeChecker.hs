module TypeChecker where

import Lexer

type Ctx = [(String, Ty)]

typeof :: Ctx -> Expr -> Maybe Ty
typeof _ (Num _) = Just TNum
typeof _ BTrue = Just TBool
typeof _ BFalse = Just TBool
typeof ctx (Add e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                           (Just TNum, Just TNum) -> Just TNum
                           _ -> Nothing
typeof ctx (And e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                           (Just TBool, Just TBool) -> Just TBool
                           _ -> Nothing
typeof ctx (Eq e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                          (Just t1, Just t2) | t1 == t2 -> Just TBool
                                             | otherwise -> Nothing
                          _ -> Nothing
typeof ctx (If e e1 e2) = case typeof ctx e of
                            Just TBool -> case (typeof ctx e1, typeof ctx e2) of
                                            (Just t1, Just t2) | t1 == t2 -> Just t1
                                                               | otherwise -> Nothing
                                            _ -> Nothing
                            _ -> Nothing
typeof ctx (List []) = Just (TList TBool) -- Tipo genérico para lista vazia
typeof ctx (List (x:xs)) = case typeof ctx x of
    Just t -> if all (\e -> typeof ctx e == Just t) xs
              then Just (TList t)
              else Nothing
    _ -> Nothing
typeof ctx (Cons h t) = case (typeof ctx h, typeof ctx t) of
    (Just th, Just (TList tt)) | th == tt -> Just (TList tt)
    _ -> Nothing
typeof ctx (Var v) = lookup v ctx
typeof ctx (Lam v t1 b) = case typeof ((v, t1) : ctx) b of
                            Just t2 -> Just (TFun t1 t2)
                            _ -> Nothing
typeof ctx (
