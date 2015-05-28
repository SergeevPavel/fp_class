-- (2 балла)
{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import Test.HUnit

import Expr
import Eval

getInt :: Eval Value -> Eval Integer
getInt m = m >>= \v -> case v of I i -> return i
                                 _   -> fail "not integer type"

getBool :: Eval Value -> Eval Bool
getBool m = m >>= \v -> case v of B b -> return b
                                  _   -> fail "not boolean type"

if' :: Eval Value -> Eval () -> Maybe (Eval ()) -> Eval ()
if' c t e = do
    cond <- getBool c
    if cond then t
            else fromMaybe mzero e

evalExpr :: Expr -> Eval Value
evalExpr (Const val) = return val
evalExpr (Var str) = getVar str
evalExpr (BinOp Plus left right) = do
    l <- getInt $ evalExpr left
    r <- getInt $ evalExpr right
    return $ I $ l + r
evalExpr (BinOp Minus left right) = do
    l <- getInt $ evalExpr left
    r <- getInt $ evalExpr right
    return $ I $ l - r
evalExpr (BinOp Mul left right) = do
    l <- getInt $ evalExpr left
    r <- getInt $ evalExpr right
    return $ I $ l * r
evalExpr (BinOp And left right) = do
    l <- getBool $ evalExpr left
    r <- getBool $ evalExpr right
    return $ B $ l && r
evalExpr (BinOp Or left right) = do
    l <- getBool $ evalExpr left
    r <- getBool $ evalExpr right
    return $ B $ l || r
evalExpr (BinOp Less left right) = do
    l <- getInt $ evalExpr left
    r <- getInt $ evalExpr right
    return $ B $ l < r
evalExpr (BinOp Greater left right) = do
    l <- getInt $ evalExpr left
    r <- getInt $ evalExpr right
    return $ B $ l > r
evalExpr (BinOp Equals left right) = do
    l <- getInt $ evalExpr left
    r <- getInt $ evalExpr right
    return $ B $ l == r
evalExpr (UnOp Neg nest) = do
    n <- getInt $ evalExpr nest
    return $ I $ negate n
evalExpr (UnOp Not nest) = do
    n <- getBool $ evalExpr nest
    return $ B $ not n

evalStatement :: Statement -> Eval ()
evalStatement (Assign str expr) = evalExpr expr >>= (update str)
evalStatement (Compound stats) = forM_ stats evalStatement
evalStatement (While expr stat) = do
    cond <- getBool $ evalExpr expr
    when cond $ evalStatement stat >> evalStatement (While expr stat)
--evalStatement


------------------------------------------------------------------------------------------------
-- tests
------------------------------------------------------------------------------------------------

test1 = not_ (Var "x") .| Var "y" .< Const (I 3) .& Var "z" .= Var "y" .&
    Const (I 5) .< Var "y" .+ Const (I 7) .* Var "z" .+ Var "y" .* Const (I 3)

test2 = neg (Const $ I 5) .+ neg (Const $ I 3) .* Const (I 2) .- Const (I 7)

test3 = Compound
    [ "r" $= Const (I 1)
    , While (Var "n" .> Const (I 0)) $ Compound
        [ "r" $= Var "r" .* Var "n"
        , "n" $= Var "n" .- Const (I 1)
        ]
    ]

main = fmap (\_ -> ()) $ runTestTT $ test
    [ TestCase $ assertBool "Expected an error" $ errorsCount (runEval (evalExpr test1) $ M.fromList [("x",I 3),("y",I 5),("f",I 5)]) > 0
    , let m = M.fromList [("x",B True),("y",I 5),("z",I 5)] in runEval (evalExpr test1) m ~?= (Just (B False), [], m)
    , let m = M.fromList [("x",B True),("y",I 2),("z",I 2)] in runEval (evalExpr test1) m ~?= (Just (B True ), [], m)
    , runEval (evalExpr test2) M.empty ~?= (Just (I (-18)), [], M.empty)
    , runEval (evalStatement test3) (M.fromList [("n",I 5)]) ~?= (Just (), [], M.fromList [("n",I 0),("r",I 120)])
    ]
  where
    errorsCount (_,es,_) = length es
