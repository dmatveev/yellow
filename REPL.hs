module REPL where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.State

import Eval


data Prompt = Default | Continuing
              deriving (Eq)

instance Show Prompt where
    show Default    = "yl> "
    show Continuing = "... "


data YLState = YLState {
    prompt :: Prompt
  , buffer :: String
  } deriving (Eq, Show)


initial :: YLState
initial = YLState { prompt = Default, buffer = "" }


reset :: (Monad m) => StateT YLState m ()
reset = do
  st <- get
  put st { buffer = "", prompt = Default }


continue :: (Monad m) => String -> StateT YLState m ()
continue src = do
  st <- get
  put st { buffer = src, prompt = Continuing }


iteration :: StateT YLState IO ()
iteration = do
  st <- get
  lift $ putStr $ show $ prompt st
  line <- lift $ getLine
  let source = buffer st ++ "\n" ++ line
  case eval' source of
     (Left (EvalError e)) ->  do lift $ putStrLn $ "Error: " ++ show e
                                 reset
     (Left (ParseError _)) -> do continue source
     (Right v)             -> do reset
                                 lift $ putStrLn $ show v
  iteration


runREPL :: IO ()
runREPL = do
  runStateT iteration initial
  return ()
