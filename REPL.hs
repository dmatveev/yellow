module REPL where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.State
import qualified Control.Monad.State as S

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


closed :: String -> S.State Int Bool
closed [] = do
  i <- S.get
  return $ i == 0

closed ('(':xs) = do
  i <- S.get
  S.put $ succ i
  closed xs
  
closed (')':xs) = do
  i <- S.get
  S.put $ pred i
  closed xs

closed (_:xs) = closed xs


isClosed :: String -> Bool
isClosed s = evalState (closed s) 0


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
     (Left e)  -> do if isClosed source
                     then do lift $ putStrLn $ "Error: " ++ show e
                             reset
                     else do continue source
     (Right r) -> do reset
                     lift $ putStrLn $ show r
  iteration


runREPL :: IO ()
runREPL = do
  runStateT iteration initial
  return ()