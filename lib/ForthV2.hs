{-# LANGUAGE OverloadedStrings #-}
module ForthV2 where

import qualified Data.Map as M
import Data.Text (Text, unpack, pack, toLower, head)
import Control.Monad.Trans.State.Lazy ( StateT (runStateT), modify, get, put, gets)
import Control.Monad.Trans.Except (Except, throwE, runExcept)
import Foreign.Marshal.Safe (with)
import Control.Monad.Trans.Class (lift)
import Data.Char (isDigit)

type StateEither s e a = StateT s (Except e) a
type ForthStateMonad a = StateEither ForthState ForthError a

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = ForthState {
      stack :: [Int]
    , customKeywords :: M.Map String [String]
} deriving Show

emptyState :: ForthState
emptyState = ForthState [] mempty

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text state
    | Data.Text.head text == ':'  = undefined
    | otherwise                   = fmap snd . runExcept . runStateT (foldState commands) $ state
    where
        commands = words . unpack . toLower $ text

foldState :: [String] -> StateT ForthState (Except ForthError) ()
foldState [] = error "Empty command"
foldState [command] = parseCommand command >> return ()
foldState (command:xs) = parseCommand command >> foldState xs

toList :: ForthState -> [Int]
toList = reverse . stack

parseCommand :: String -> ForthStateMonad ()
parseCommand command = do
    keywords <- gets customKeywords
    case command `M.lookup` keywords of
        Nothing -> parseCommand' command
        Just a -> undefined

parseCommand' :: String -> StateT ForthState (Except ForthError) ()
parseCommand' "+" = applyOnStack (+)
parseCommand' "-" = applyOnStack (-)
parseCommand' "*" = applyOnStack (*)
parseCommand' "/" = do
            x <- getItem
            if x == 0 then
                throwForthError DivisionByZero
            else
                applyOnStack (div)
parseCommand' "dup" = getItem >>= putItem
parseCommand' "drop" = popItem >> return ()
parseCommand' "over" = do
            (x, y) <- pop2Items
            putItem y
            putItem x
            putItem y
parseCommand' "swap" = do
            (x, y) <- pop2Items
            putItem x
            putItem y 
parseCommand' command = 
    if all isDigit command then 
        putItem . read $ command 
    else 
        throwForthError $ UnknownWord (pack command)

throwForthError = lift . throwE

applyOnStack op = liftA2 (flip op) popItem popItem >>= putItem

putItem :: Int -> ForthStateMonad ()
putItem x = modify (fmapStack ((:) x))

getItem :: ForthStateMonad Int
getItem = do
    stack <- gets stack
    case stack of
        [] -> lift . throwE $ StackUnderflow
        (x:_) -> return x

pop2Items :: ForthStateMonad (Int, Int)
pop2Items = do
    x <- popItem
    y <- popItem
    return (x, y)

popItem :: ForthStateMonad Int
popItem = do
    stack <- gets stack
    case stack of
        [] -> lift . throwE $ StackUnderflow
        (x:_) -> modify (fmapStack (drop 1))>> return x


fmapStack :: ([Int] -> [Int]) -> ForthState -> ForthState
fmapStack fn (ForthState stack map) = ForthState (fn stack) map