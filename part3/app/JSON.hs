{-# OPTIONS_GHC -Wno-x-partial #-}
{-# LANGUAGE OrPatterns #-}

module JSON where

import Control.Monad.State
import Data.Bifunctor (first, Bifunctor (second, bimap))
import qualified Data.Map as M

type Object = M.Map String JSONValue

data JSONValue =
    JSONObject Object
    | JSONArray [JSONValue]
    | JSONString String
    | JSONNumber Double
    deriving Show

type Parse = State String JSONValue

parseJson :: String -> JSONValue
parseJson = evalState parseObject

parseKeyVal :: State String (String, JSONValue)
parseKeyVal = do
    modify eatWhitespace
    key <- getName
    modify eatWhitespace
    next <- gets checkNext
    val <- parseValue $ fst next

    modify eatWhitespace
    modify tryEatComma
    return (key, val)

eatWhitespace :: String -> String
eatWhitespace ((' ';'\n'):rest) = eatWhitespace rest
eatWhitespace rest = rest

tryEatComma :: String -> String
tryEatComma (',':rest) = rest
tryEatComma rest = rest

getName :: State String String
getName = state eatName
    where
        eatName :: String -> (String, String)
        eatName (':':cs) = ([], cs)
        eatName (c:cs) = first (c:) $ eatName cs
        eatName _ = error "why"

checkNext :: String -> (Char, String)
checkNext str = (head str, str)

getNext :: State String ()
getNext = modify $ drop 1

parseValue :: Char -> State String JSONValue
parseValue next = case next of
  '{' -> getNext >> parseObject
  '[' -> getNext >> parseList
  '"' -> getNext >> parseString
  _ -> parseNumber

parseObject :: State String JSONValue
parseObject = do
    current <- get
    let (vals, st') = runState _parseObject (current, [])
    put $ fst st'
    getNext
    return $ JSONObject $ M.fromList vals
    where
        _parseObject :: State (String, [(String, JSONValue)]) [(String, JSONValue)]
        _parseObject = do
            (cs, current) <- get
            if head cs == '}' then return current
            else do 
                let (val, st') = runState parseKeyVal cs
                modify $ second (val:)
                modify $ first $ pure st'
                _parseObject

parseList :: State String JSONValue
parseList = do
    current <- get
    let (vals, st') = runState _parseList (current, [])
    put $ fst st'
    getNext
    return $ JSONArray vals
    where
        _parseList :: State (String, [JSONValue]) [JSONValue]
        _parseList = do
            modify $ first eatWhitespace
            (cs, current) <- get
            if head cs == ']' then return $ reverse current
            else do
                let (val, st') = runState (parseValue $ head cs) cs
                modify $ first $ pure st'
                modify $ first eatWhitespace
                modify $ first tryEatComma
                modify $ second (val:)
                _parseList

parseString :: State String JSONValue
parseString = do
    current <- get
    let (val, st') = runState _parseString (current, [])
    put $ fst st'
    getNext
    return $ JSONString val
    where
        _parseString :: State (String, String) String
        _parseString = do
            (cs, current) <- get
            if head cs == '"' then return $ reverse current
            else do
                modify $ bimap (drop 1) (head cs:)
                _parseString

parseNumber :: State String JSONValue
parseNumber = do
    current <- get
    let 
        (sign, st) = checkSign current
        (intPart, (st', _)) = runState _parseInteger (st, "")
        (fracPart, (st'', _)) = if head st' == '.' then runState _parseInteger (drop 1 st', "") else ("0", (st', ""))
        (expPart, (st''', _)) = if head st'' == 'e' then let (expsign, st'''') = checkSign (drop 1 st'')
                                                    in  first ((expsign *) . normalParse) $ runState _parseInteger (st'''', "")
                                               else (0, (st'', ""))
    put st'''
    return $ JSONNumber $ sign * (normalParse intPart + fracParse fracPart) * 10 ** expPart

    where
        checkSign :: String -> (Double, String)
        checkSign st = case head st of
                        '+' -> (1, drop 1 st)
                        '-' -> (-1, drop 1 st)
                        _ -> (1, st)

        normalParse = read
        fracParse = read . ("0." ++)

        _parseInteger :: State (String, String) String
        _parseInteger = do
            (next, current) <- get
            if head next < '0' || head next > '9' then return $ reverse current
            else do
                modify $ bimap (drop 1) (head next:)
                _parseInteger
