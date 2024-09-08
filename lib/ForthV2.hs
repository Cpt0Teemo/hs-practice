{-# LANGUAGE OverloadedStrings #-}
module ForthV2 where

import qualified Data.Map as M
import Data.Text (Text, unpack, pack, toLower, head)
import Control.Monad.Trans.State.Lazy ( StateT (runStateT), modify, get, put, gets)
import Control.Monad.Trans.Except (Except, throwE, runExcept)
import Foreign.Marshal.Safe (with)
import Control.Monad.Trans.Class (lift)
import Data.Char (isDigit, isAlpha)
import qualified Data.Text as T

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
    | Data.Text.head text == ':'  = 
        case commands of
            (":" : keyword : xs) | validKeyword keyword -> do
                                    commands' <- replaceKeywordsWithCommands (customKeywords state) . init $ xs
                                    return $ addKeyword keyword commands' state
            _                    -> Left InvalidWord
    | otherwise                   = fmap snd . runExcept . runStateT (foldState commands) $ state
    where
        commands = words . unpack . toLower $ text
        validKeyword = not . isDigit . Prelude.head
        addKeyword keyword commands' state' = state { customKeywords = M.insert keyword commands' (customKeywords state')}

replaceKeywordsWithCommands :: M.Map String [String] -> [String] -> Either ForthError [String]
replaceKeywordsWithCommands customKeywords commands = fmap (foldl (<>) []) . traverse replaceKeywordsWithCommands $ commands
    where
        allowedKeywords = ["+","-","*","/","dup","over","drop","swap"]
        replaceKeywordsWithCommands command
            | all isDigit command || command `elem` allowedKeywords = return [command]
            | M.member command customKeywords = return . (M.!) customKeywords $ command
            | otherwise = Left . UnknownWord . T.pack $ command 

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
        Just commands -> foldState commands

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