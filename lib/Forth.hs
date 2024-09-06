module Forth where

import qualified Data.Text as T
import Data.List
import Data.Char ( isDigit, isAlpha, isAscii )
import qualified Data.HashMap.Strict as Map
import Control.Arrow ( Arrow(first), (>>>) )
import Control.Monad (foldM, (>>=))
import Control.Applicative (liftA2)
import Data.Either (fromRight)
import Debug.Trace (trace)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord T.Text
     deriving (Show, Eq)

data BaseCommand
    = Dup
    | Drop 
    | Swap
    | Over
    | Operator Char -- +, -, *, /
    | Value Int
    deriving Show

data Command = BaseCommand BaseCommand | CustomKeyword String

data ForthState = ForthState {
      stack :: [Int]
    , customKeywords :: Map.HashMap String [BaseCommand]
} deriving Show

updateStateStack :: ForthState -> [Int] -> ForthState
updateStateStack (ForthState _ customKeywords) newStack = ForthState newStack customKeywords 
updateStateStackFn :: (Monad f) => ([Int] -> f [Int]) -> ForthState -> f ForthState
updateStateStackFn fn (ForthState stack customKeywords) = liftA2 ForthState (fn stack) (return customKeywords)
addKeywordToState :: ForthState -> String -> [BaseCommand] -> ForthState
addKeywordToState (ForthState stack customKeywords) k v = ForthState stack (Map.insert k v customKeywords)


forthKeywords :: [String]
forthKeywords = ["dup", "drop", "swap", "over"]

emptyState :: ForthState
emptyState = ForthState [] Map.empty

evalText :: T.Text -> ForthState -> Either ForthError ForthState
evalText text stack 
    | T.length text > 0 && T.head text == ':' =
        T.toLower
          >>> T.unpack
          >>> words
          >>> evalWord stack
          $ text
    | otherwise = 
        T.toLower
          >>> T.unpack
          >>> words
          >>> mapM (parseCommand keywords)
          >>> fmap concat
          >>> (>>= evalCommands stack)
          $ text
    where keywords = customKeywords stack


simplified :: ForthState -> T.Text -> Either ForthError ForthState
simplified stack = T.toLower
          >>> T.unpack
          >>> words
          >>> evalWord stack

toList :: ForthState -> [Int]
toList = reverse . stack

evalWord :: ForthState -> [String] -> Either ForthError ForthState
evalWord state (":" : keyword : xs)
    | all (not . isDigit) keyword = 
          init
        >>> mapM (parseCommand (customKeywords state))
        >>> fmap concat
        >>> fmap (addKeywordToState state keyword)
        $ xs
    | otherwise           = Left InvalidWord  
evalWord _ _ = Left InvalidWord

parseCommand :: Map.HashMap String [BaseCommand] -> String -> Either ForthError [BaseCommand]
parseCommand keywordMap x
    | all isDigit x     = return . singleton . Value . read $ x
    | Map.member x keywordMap  = return $ (Map.!) keywordMap x
    | otherwise         = parseCommand' keywordMap x
    where
        parseCommand' _ "+" = return . singleton $ Operator '+'
        parseCommand' _ "-" = return . singleton $ Operator '-'
        parseCommand' _ "*" = return . singleton $ Operator '*'
        parseCommand' _ "/" = return . singleton $ Operator '/'
        parseCommand' _ "dup" = return . singleton $ Dup
        parseCommand' _ "drop" = return . singleton $ Drop
        parseCommand' _ "over" = return . singleton $ Over
        parseCommand' _ "swap" = return . singleton $ Swap
        parseCommand' _ x = Left (UnknownWord (T.pack x))

evalCommands :: ForthState -> [BaseCommand] -> Either ForthError ForthState
evalCommands = foldM (flip evalCommand)

evalCommand :: BaseCommand -> ForthState -> Either ForthError ForthState
evalCommand Dup             = duplicateCommand
evalCommand Drop            = dropCommand
evalCommand Over            = overCommand
evalCommand Swap            = swapCommand
evalCommand (Value x)       = addCommand x
evalCommand (Operator op)   = operatorCommand op
evalCommand _ = undefined


addCommand :: Int -> ForthState -> Either ForthError ForthState
addCommand x = updateStateStackFn (return . (:) x)

duplicateCommand :: ForthState -> Either ForthError ForthState
duplicateCommand = updateStateStackFn $ mapOnFstNElements doubleSingleton 1
    where
        doubleSingleton [x] = [x,x]

dropCommand :: ForthState -> Either ForthError ForthState
dropCommand = updateStateStackFn $ mapOnFstNElements tail 1

swapCommand :: ForthState -> Either ForthError ForthState
swapCommand = updateStateStackFn $ mapOnFstNElements reverse 2

overCommand :: ForthState -> Either ForthError ForthState
overCommand = updateStateStackFn $ mapOnFstNElements prependSnd 2
    where 
        prependSnd [x,y] = [y,x,y]

operatorCommand :: Char -> ForthState -> Either ForthError ForthState
operatorCommand '+' = updateStateStackFn $ mapOnFstNElements (singleton . sum) 2
operatorCommand '-' = updateStateStackFn $ mapOnFstNElements (singleton . foldr1 (-) . reverse) 2
operatorCommand '*' = updateStateStackFn $ mapOnFstNElements (singleton . foldr1 (*)) 2
operatorCommand '/' = updateStateStackFn $ eitherMapOnFstNElements (\[x,y] -> fmap singleton . safeDiv y $ x) 2
operatorCommand _ = error "Unsupported operator"

mapOnFstNElements :: ([a] -> [a]) -> Int -> [a] -> Either ForthError [a]
mapOnFstNElements fn n = eitherMapOnFstNElements (return . fn) n

eitherMapOnFstNElements :: ([a] -> Either ForthError [a]) -> Int -> [a] -> Either ForthError [a]
eitherMapOnFstNElements fn n xs = do
    (l1, l2) <- maybeToEither StackUnderflow .  safeSplitAfter n $ xs
    l1'      <- fn l1
    return $ l1' ++ l2

mapOnFstNElements' :: ([a] -> [a]) -> Int -> [a] -> Either ForthError [a]
mapOnFstNElements' fn n = fmap join . fmap (first fn) . maybeToEither DivisionByZero .  safeSplitAfter n

-------- Utils

maybeToEither :: ForthError ->  Maybe a -> Either ForthError a
maybeToEither error Nothing = Left error
maybeToEither _ (Just x) = return x

safeDiv :: Int -> Int -> Either ForthError Int
safeDiv _ 0 = Left DivisionByZero
safeDiv x y = return (x `div` y)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeTake :: Int -> [a] -> Maybe [a]
safeTake 0 _ = return []
safeTake n xs
    | n < 0 = Nothing
    | otherwise = liftA2 (:) (safeHead xs) (safeTake (n-1) (drop 1 xs))

safeDrop :: Int -> [a] -> Maybe [a]
safeDrop 0 xs = return xs
safeDrop n xs
    | n < 0 = Nothing
    | otherwise = (safeTail xs) >>= (safeDrop (n-1))

safeSplitAfter :: Int -> [a] -> Maybe ([a], [a])
safeSplitAfter n xs = liftA2 (\x y -> (x, y)) (safeTake n xs) (safeDrop n xs)

join :: ([a], [a]) -> [a]
join (l1, l2) = l1 ++ l2
