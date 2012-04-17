module Eval where


import Form

data EvalError = EvalError String
               | ParseError String
                 deriving (Eq, Show)


eval :: Form -> Either EvalError Form
eval (SExpr o fs) = evalSExpr o fs
eval form         = return $ form
    

eval' :: String -> Either EvalError Form
eval' s = case parseTree s of
  (Left e)  -> Left $ ParseError e
  (Right t) -> eval t

    
evalSExpr :: Operation -> [Form] -> Either EvalError Form
evalSExpr op = case op of
    "+"    -> eval1Plus $ evalArith (+)
    "*"    -> eval1Plus $ evalArith (*)
    "-"    -> eval1Plus evalSubst
    
    "/"    -> evalBinary evalDiv
    "="    -> evalBinary evalEqual
    
    "if"   -> eval1Plus evalIf
    
    "cons" -> evalBinary evalCons
    "car"  -> evalUnary  evalCar
    "cdr"  -> evalUnary  evalCdr


evalArith :: (Double -> Double -> Double) -> [Form] -> Either EvalError Form
evalArith f (arg : []) = do
  value <- numeric =<< eval arg
  return $ NumericLiteral $ value
  
evalArith f (arg : args) = do
  left  <- numeric =<< eval arg
  right <- numeric =<< evalArith f args
  return $ NumericLiteral $ f left right


evalSubst :: [Form] -> Either EvalError Form
evalSubst [a]    = negated =<< eval a
evalSubst (a:fs) = do
  l <- numeric =<< eval a
  r <- numeric =<< evalArith (+) fs
  return $ NumericLiteral $ l - r


evalUnary :: (Form -> Either EvalError Form)
          -> [Form]
          -> Either EvalError Form
evalUnary e (a:[]) = e a             
evalUnary _ _      = wrongNumberOfArgs


evalBinary :: (Form -> Form -> Either EvalError Form)
           -> [Form]
           -> Either EvalError Form
evalBinary e (a:b:[]) = e a b
evalBinary _ _        = wrongNumberOfArgs


eval1Plus :: ([Form] -> Either EvalError Form)
          -> [Form]
          -> Either EvalError Form
eval1Plus _ [] = wrongNumberOfArgs
eval1Plus e xs = e xs

  
evalDiv :: Form -> Form -> Either EvalError Form
evalDiv a b = do
  a' <- numeric =<< eval a 
  b' <- numeric =<< eval b
  return $ NumericLiteral $ a' / b'


evalEqual :: Form -> Form -> Either EvalError Form
evalEqual a b = do
  a' <- eval a
  b' <- eval b
  return $ BooleanLiteral $ a' == b'
  
  
evalIf :: [Form] -> Either EvalError Form
evalIf (exp : thenForm : elseForm : _ ) = do
  e <- eval exp
  eval $ if e /= nil then thenForm else elseForm
    
evalIf (exp : thenForm : [] ) = evalIf [exp, thenForm, nil]
evalIf _ = wrongNumberOfArgs


negated :: Form -> Either EvalError Form
negated f = do
  i <- numeric =<< eval f
  return $ NumericLiteral $ 0 - i


evalCons :: Form -> Form -> Either EvalError Form
evalCons h t = do
  h' <- eval h
  t' <- eval t
  return $ Cons h' t'


evalCar :: Form -> Either EvalError Form
evalCar f = eval f >>= cons >>= \(h, _) -> return h


evalCdr :: Form -> Either EvalError Form
evalCdr f = eval f >>= cons >>= \(_, t) -> return t


wrongNumberOfArgs :: Either EvalError Form
wrongNumberOfArgs = Left $ EvalError "wrong number of arguments"


numeric :: Form -> Either EvalError Double
numeric (NumericLiteral i) = return i
numeric _                  = Left $ EvalError "number expected"


cons :: Form -> Either EvalError (Form, Form)
cons (Cons h t) = return $ (h, t)
cons _ = Left $ EvalError "cons expected"
