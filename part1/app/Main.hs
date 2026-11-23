{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LexicalNegation #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import qualified Data.ByteString as B
import Data.Bits ((.&.), shiftR, Bits (complement, shiftL, xor))
import Data.Binary (Word8, Word16)
import Text.Printf (printf)
import Debug.Trace (trace)
import Data.Map ((!), fromList)
import qualified Data.Map as M
import System.Environment (getArgs)
import Lens.Micro (set, lens, over, Lens, (^.))
import Lens.Micro.TH (makeLenses)
import Data.Array.Base (UArray, array)
import qualified Data.Array.Base as U
import qualified Data.List as L
import Data.Maybe (fromMaybe, fromJust, isJust)
import Control.Monad.ST (runST)
import GHC.ST (ST())

tracer :: Show a => a -> a
tracer a = trace (show a) a

type MemLens = Lens DisassemblerData DisassemblerData Int Int
type FlagLens = Lens DisassemblerData DisassemblerData Bool Bool

data Optype = Rm (String, Int) | Immediate [(String, Int)] | Logic [(String, Int)]

eqTable :: [[Char]]
eqTable = [
    "bx + si"
    , "bx + di"
    , "bp + si"
    , "bp + di"
    , "si"
    , "di"
    , "bp"
    , "bx"
    ]

data Addressifier = Addressifier {
    displacement :: Int
    , eqType :: Int
    , segReg :: Maybe String
    , segOffset :: Int
}

instance Show Addressifier where
    show (Addressifier displacement eqType segReg segOffset) =
        (if isJust segReg then fromJust segReg ++ ":" else "")
        ++ "["
        ++ (if segOffset /= 0 then show segOffset ++ ":" else "")
        ++ (if eqType /= -1 then eqTable !! eqType else "")
        ++ (if displacement /= 0 || eqType /= 6
                then (case (displacement >= 0, eqType) of
                        (True, -1) -> ""
                        (False, -1) -> "-"
                        (True, _) -> " + "
                        (False, _) -> " - ")
                    ++ show (abs displacement)
                else "")
        ++ "]"

data Argtype = Register String | Value Int | Mem Addressifier
instance Show Argtype where
    show this = case this of
        (Register n) -> n
        (Value i) -> show i
        (Mem addressifier) -> show addressifier

type Memory = UArray Int Word8

data DisassemblerData = DisassemblerData {
    _registers :: [Word8]
    , _flags :: [Bool]
    , _lock :: Bool
    , _segmentOverride :: Maybe Int
    , _memory :: Memory
    , _codeLength :: Word16
    , _ip :: Word16
    , _out :: [String]
    , _totalClocks :: Int
} deriving Show

$(makeLenses ''DisassemblerData)

getFlagLens :: Int -> FlagLens
getFlagLens offset = lens ((!! offset) . _flags) (\ s v -> over flags (\ ov -> take offset ov ++ [v] ++ drop (offset + 1) ov) s)

carryFlag :: FlagLens
carryFlag = getFlagLens 0

parityFlag :: FlagLens
parityFlag = getFlagLens 1

auxiliaryCarryFlag :: FlagLens
auxiliaryCarryFlag = getFlagLens 2

zeroFlag :: FlagLens
zeroFlag = getFlagLens 3

signFlag :: FlagLens
signFlag = getFlagLens 4

overflowFlag :: FlagLens
overflowFlag = getFlagLens 5

getRegLens :: Int -> Int -> MemLens
getRegLens offset bytes = lens getter setter
    where
        getter state = foldr (\ a b -> (fromIntegral (_registers state !! fromIntegral (offset + a)) `shiftL` (a * 8)) + b) 0 [0..(bytes - 1)]
        setter state val = over registers (\ r -> take offset r ++ getValArr bytes val ++ drop (offset + bytes) r) state
            where
                getValArr 0 _ = []
                getValArr bytes v = fromIntegral (v .&. 0xff) : getValArr (bytes - 1) (v `shiftR` 8)

displayFlags :: DisassemblerData -> String
displayFlags state = "[" ++ [x | (x, y) <- zip (map head flagNames) (_flags state), y] ++ "]"

lensFromName :: String -> MemLens
lensFromName name = fromList [
    ("ax", getRegLens 0 2)
    , ("bx", getRegLens 2 2)
    , ("cx", getRegLens 4 2)
    , ("dx", getRegLens 6 2)
    , ("al", getRegLens 0 1)
    , ("bl", getRegLens 2 1)
    , ("cl", getRegLens 4 1)
    , ("dl", getRegLens 6 1)
    , ("ah", getRegLens 1 1)
    , ("bh", getRegLens 3 1)
    , ("ch", getRegLens 5 1)
    , ("dh", getRegLens 7 1)
    , ("si", getRegLens 8 2)
    , ("di", getRegLens 10 2)
    , ("bp", getRegLens 12 2)
    , ("sp", getRegLens 14 2)
    , ("es", getRegLens 16 2)
    , ("cs", getRegLens 18 2)
    , ("ss", getRegLens 20 2)
    , ("ds", getRegLens 22 2)
    ] ! name

resolveEqType :: Int -> DisassemblerData -> Int
resolveEqType eqType state = fromMaybe 0 $ [
    state ^. lensFromName "bx" + state ^. lensFromName "si"
    , state ^. lensFromName "bx" + state ^. lensFromName "di"
    , state ^. lensFromName "bp" + state ^. lensFromName "si"
    , state ^. lensFromName "bp" + state ^. lensFromName "di"
    , state ^. lensFromName "si"
    , state ^. lensFromName "di"
    , state ^. lensFromName "bp"
    , state ^. lensFromName "bx"
    ] L.!? eqType

getLocation :: Addressifier -> DisassemblerData -> Int
getLocation addressifier state =
    displacement addressifier 
    + resolveEqType (eqType addressifier) state
    + (case segReg addressifier of
        (Just name) -> (state ^. lensFromName name) `shiftL` 4
        _ -> (state ^. lensFromName "ds") `shiftL` 4)

resolveArg :: Argtype -> Word8 -> DisassemblerData -> MemLens
resolveArg arg w s = case arg of
    (Register name) -> lensFromName name
    (Value num) -> lens (pure num) (\ _ _ -> error "why try set value????")
    (Mem addressifier) -> lens
        ((\ memory -> foldr (\ a b -> b + fromIntegral (memory U.! (location + a)) `shiftL` (a * 8)) 0 spots) . _memory)
        (\ s v -> over memory (\memory -> foldr unsafeWrite memory $ zip (map (+ location) spots) ([fromIntegral $ v `shiftR` (i * 8) | i <- spots])) s)
        where
            location = getLocation addressifier s
            winted = fromIntegral w
            spots = [0..winted]

            unsafeWrite :: (Int, Word8) -> UArray Int Word8 -> UArray Int Word8
            unsafeWrite (i, v) arr  = runST $ do
                a <- U.unsafeThaw arr :: ST s (U.STUArray s Int Word8)
                U.writeArray a i v
                U.unsafeFreeze a

addTrace :: String -> DisassemblerData -> DisassemblerData
addTrace str state = over out (((lastOut ++ (if ';' `elem` lastOut then ", " else " ; ") ++ str) :) . tail) state
    where
        lastOut = head $ _out state

getName :: Argtype -> DisassemblerData -> String
getName arg state = case arg of
    (Register name) -> name
    (Mem addressifier) -> "[" ++ show (getLocation addressifier state) ++ "]"
    (Value _) -> error "why try get value name"

flagNames :: [String]
flagNames = ["C", "P", "A", "Z", "S", "O", "I", "D", "T"]

outputFlagDiff :: DisassemblerData -> DisassemblerData -> DisassemblerData
outputFlagDiff oldState state = if displayFlags oldState /= displayFlags state then addTrace ("flags <== " ++ displayFlags state) state else state

setArithmeticFlags :: Word8 -> Int -> DisassemblerData -> DisassemblerData
setArithmeticFlags w valFinal =
    set signFlag (lastBit w valFinal)
    . set zeroFlag (valFinal .&. (0xff + 0xff00 * fromIntegral w) == 0)
    . set parityFlag (even $ parity (fromIntegral w * 8 - (if signBit then 2 else 1)) + (if signBit then 1 else 0))
    where
        parity (-1) = 0
        parity offset = (if odd $ valFinal `shiftR` offset then 1 else 0) + parity (offset - 1)
        signBit = w == 1 && valFinal < 0

lastBit :: Word8 -> Int -> Bool
lastBit w val = odd $ val `shiftR` (fromIntegral w * 8 + 7)

doAdd :: MemLens -> Word8 -> Int -> Int -> DisassemblerData -> DisassemblerData
doAdd lens w val1 val2 state =
    outputFlagDiff state
    . set carryFlag carryFlagVal
    . set overflowFlag overflowFlagVal
    . set auxiliaryCarryFlag ((val1 .&. 0xf + val2 .&. 0xf) `shiftR` 4 > 0)
    . setArithmeticFlags w finalVal
    . set lens finalVal
    $ state
    where
        finalVal = val1 + val2
        carryFlagVal = finalVal `shiftR` (fromIntegral w * 8 + 8) > 0
        overflowFlagVal = lastBit w val1 == lastBit w val2 && lastBit w val1 /= lastBit w finalVal

doSub :: MemLens -> Word8 -> Int -> Int -> DisassemblerData -> DisassemblerData
doSub lens w val1 val2 state =
    outputFlagDiff state
    . set carryFlag carryFlagVal
    . set overflowFlag overflowFlagVal
    . set auxiliaryCarryFlag ((val1 .&. 0xf - val2 .&. 0xf) `shiftR` 4 < 0)
    . setArithmeticFlags w finalVal
    . set lens finalVal
    $ state
    where
        finalVal = val1 + twosComplement w (negate $ unTwosComplement w val2)
        carryFlagVal = val1 - val2 < 0
        overflowFlagVal = lastBit w val1 /= lastBit w val2 && lastBit w val2 == lastBit w finalVal


mov :: Word8 -> [Argtype] -> DisassemblerData -> DisassemblerData
mov w [arg1, arg2] state = addTrace (getName arg1 state ++ " <== " ++ show val) $ set lens val state
    where
        val = state ^. resolveArg arg2 w state
        lens = resolveArg arg1 w state
mov _ _ _ = error "help"

add :: Word8 -> [Argtype] -> DisassemblerData -> DisassemblerData
add w [arg1, arg2] state =
    doAdd regLens w val1 val2
    . addTrace (getName arg1 state ++ " <== " ++ show val1 ++ " + " ++ show val2)
    $ state
    where
        regLens :: MemLens
        regLens = resolveArg arg1 w state
        val2 = state ^. resolveArg arg2 w state
        val1 = state ^. regLens
add _ _ _ = error "help"

sub :: Word8 -> [Argtype] -> DisassemblerData -> DisassemblerData
sub w [arg1, arg2] state =
    doSub regLens w val1 val2
    . addTrace (getName arg1 state ++ " <== " ++ show val1 ++ " - " ++ show val2)
    $ state
    where
        regLens :: MemLens
        regLens = resolveArg arg1 w state
        val2 = state ^. resolveArg arg2 w state
        val1 = state ^. regLens
sub _ _ _ = error "help"

cmp :: Word8 -> [Argtype] -> DisassemblerData -> DisassemblerData
cmp w [arg1, arg2] state =
    outputFlagDiff state
    . setArithmeticFlags w (val1 - val2)
    $ state
    where
        regLens :: MemLens
        regLens = resolveArg arg1 w state
        val1 = state ^. regLens
        val2 = state ^. resolveArg arg2 w state
cmp _ _ _ = error "help"

condJmp :: (DisassemblerData -> Bool) -> Word8 -> [Argtype] -> DisassemblerData -> DisassemblerData
condJmp condition w [arg] state = case arg of
    (Value val) ->
        addTrace ("ip <== " ++ show (_ip state) ++ " + " ++ show signedVal)
        . (if condition state then over ip (+ fromIntegral signedVal) else id)
        $ state
        where
            signedVal = unTwosComplement w val
    _ -> error "Non number jump val?"
condJmp _ _ _ _ = error "nooooo"

loopJmp :: (DisassemblerData -> Bool) -> Word8 -> [Argtype] -> DisassemblerData -> DisassemblerData
loopJmp condition w args state = condJmp (\ s -> s ^. lensFromName "cx" /= 0 && condition s) w args $ over (lensFromName "cx") (- 1) state

nop :: p1 -> p2 -> p3 -> p3
nop _ _ state = state

opFromName :: String -> (Word8 -> [Argtype] -> DisassemblerData -> DisassemblerData)
opFromName name = case fromList [
    ("mov", mov)
    , ("add", add)
    , ("sub", sub)
    , ("cmp", cmp)
    , ("jnz", condJmp (not . (^. zeroFlag)))
    , ("je", condJmp (^. zeroFlag))
    , ("jl", condJmp (\ s -> s ^. signFlag /= s ^. overflowFlag))
    , ("jle", condJmp (\ s -> s ^. signFlag /= s ^. overflowFlag || s ^. zeroFlag))
    , ("jb", condJmp (^. carryFlag))
    , ("jbe", condJmp (\ s -> s ^. carryFlag || s ^. zeroFlag))
    , ("jp", condJmp (^. parityFlag))
    , ("jo", condJmp (^. overflowFlag))
    , ("js", condJmp (^. signFlag))
    , ("jge", condJmp (\ s -> s ^. signFlag == s ^. overflowFlag))
    , ("jg", condJmp (\ s -> s ^. signFlag == s ^. overflowFlag && not (s ^. zeroFlag)))
    , ("jae", condJmp (not . (^. carryFlag)))
    , ("ja", condJmp (\ s -> not (s ^. carryFlag) && not (s ^. signFlag)))
    , ("jnp", condJmp (not . (^. parityFlag)))
    , ("jno", condJmp (not . (^. overflowFlag)))
    , ("jns", condJmp (not . (^. signFlag)))
    , ("loop", loopJmp (pure True))
    , ("loope", loopJmp (^. zeroFlag))
    , ("loopnz", loopJmp (not . (^. zeroFlag)))
    , ("jcxz", condJmp ((== 0) . (^. lensFromName "cx")))
    ] M.!? name of
        (Just v) -> v
        Nothing -> error $ "Unexpected instruction name: " ++ name

emitDisassemblerInstruction :: Int -> ([Char], Word16) -> DisassemblerData -> DisassemblerData
emitDisassemblerInstruction clocks (s, m) this =
    set lock False
    . set segmentOverride Nothing
    . set ip m
    . over out ((toAdd ++ "; " ++ show clocks) :)
    . over totalClocks (+ clocks)
    $ this
    where
        toAdd = (if _lock this then "lock" else "") ++ s

biOpTable :: Word8 -> [(String, Int)]
biOpTable mod = [
    ("add", if mod == 0b11 then 4 else 17)
    , ("or", 0)
    , ("adc", 0)
    , ("sbb", 0)
    , ("and", 0)
    , ("sub", 0)
    , ("xor", 0)
    , ("cmp", 0)
    ]

stackTable :: [(String, Int)]
stackTable = [
    ("inc", 0)
    , ("dec", 0)
    , ("call", 0)
    , ("call far", 0)
    , ("jmp", 0)
    , ("jmp far", 0)
    , ("push", 0)
    ]

unOpTable :: [(String, Int)]
unOpTable = [
    ("test", 0)
    , ("what", 0)
    , ("not", 0)
    , ("neg", 0)
    , ("mul", 0)
    , ("imul", 0)
    , ("div", 0)
    , ("idiv", 0)
    ]

logicTable :: [(String, Int)]
logicTable = [
    ("rol", 0)
    , ("ror", 0)
    , ("rcl", 0)
    , ("rcr", 0)
    , ("shl", 0)
    , ("shr", 0)
    , ("what", 0)
    , ("sar", 0)
    ]

main :: IO ()
main = do
    args <- getArgs
    bytes <- B.readFile (head args)
    let disassemblerData = DisassemblerData {
        _registers=[if x < 16 then 0 else x | x <- [0..15] ++ [0, 0b00100000, 0, 0, 0, 0b00010000, 0, 0]]
        , _flags=[False | _ <- [0..8]]
        , _lock=False
        , _segmentOverride=Nothing
        , _memory=array (0, 1048576) $ zip [0..] $ B.unpack bytes
        , _codeLength=fromIntegral $ B.length bytes
        , _ip=0
        , _out=[]
        , _totalClocks=0}
    putStr $ "bits 16\n\n" ++ foldr (\ a b -> a ++ "\n" ++ b) "" (runOp disassemblerData)
    B.writeFile "dump.data" (B.pack $ U.elems $ _memory disassemblerData)

runOp :: DisassemblerData -> [String]
runOp state = if _ip state < _codeLength state
    then let state' = dispatchOp state
         in runOp state'
    else reverse (_out state)
         ++ [] : ("; cycles: " ++ show (_totalClocks state))
         : [] : displayEndState (drop 8 regTable ++ take 4 (drop 8 segRegTable)) state
         ++ [] : ("; flags: " ++ displayFlags state)
         : ("; ip: " ++ show (_ip state)) 
         : [] : "; memory: " 
         : displayMemory state

displayEndState :: [String] -> DisassemblerData -> [String]
displayEndState (regName:rest) state = ("; " ++ regName ++ ": " ++ show (state ^. lensFromName regName)) : displayEndState rest state
displayEndState _ _ = []

displayMemory :: DisassemblerData -> [[Char]]
displayMemory state = displayMemory_ (_codeLength state) (_memory state)

displayMemory_ :: Word16 -> Memory -> [[Char]]
displayMemory_ codeLength memory = ["; " ++ show (i, x) | (i, x) <- zip [0..] (U.elems memory), i > fromIntegral codeLength, x /= 0]

dispatchOp :: DisassemblerData -> DisassemblerData
dispatchOp state
    | checkOp 0b100010 2 =  binaryOp    (Rm ("mov", 
        if mod == 0b11 then 2 else if d == 0 then 9 else 8))         0   d   w   mod     reg rm      regTable    read2
    | checkOp 0b1100011 1 = binaryOp    (Immediate [("mov", 10)])    0   0   w   mod     reg rm      regTable    read2
    | checkOp 0b101000 2 =  binaryOp    (Rm ("mov", 10))             0   d   w   0b00    0   0b110   regTable    read1
    | checkOp 0b10001100 0 =binaryOp    (Rm ("mov", 
        if mod == 0b11 then 2 else 8))                               0   d   1   mod     reg rm      segRegTable read2
    | checkOp 0b10001110 0 =binaryOp    (Rm ("mov", 
        if mod == 0b11 then 2 else 9))                               0   d   1   mod     reg rm      segRegTable read2
    | checkOp 0b100000 2 =  binaryOp    (Immediate $ biOpTable mod)  d   0   w   mod     reg rm      regTable    read2
    | checkOp 0b1000011 1 = binaryOp    (Rm ("xchg", 0))             0   0   w   mod     reg rm      regTable    read2
    | checkOp 0b10001101 0 =binaryOp    (Rm ("lea", 0))              0   1   1   mod     reg rm      regTable    read2
    | checkOp 0b11000101 0 =binaryOp    (Rm ("lds", 0))              0   1   1   mod     reg rm      regTable    read2
    | checkOp 0b11000100 0 =binaryOp    (Rm ("les", 0))              0   1   1   mod     reg rm      regTable    read2
    | checkOp 0b000000 2 =  binaryOp    (Rm ("add", 
        if mod == 0b11 then 3 else if d == 0 then 16 else 9))        0   d   w   mod     reg rm      regTable    read2
    | checkOp 0b000100 2 =  binaryOp    (Rm ("adc", 0))              0   d   w   mod     reg rm      regTable    read2
    | checkOp 0b001010 2 =  binaryOp    (Rm ("sub", 0))              0   d   w   mod     reg rm      regTable    read2
    | checkOp 0b000110 2 =  binaryOp    (Rm ("sbb", 0))              0   d   w   mod     reg rm      regTable    read2
    | checkOp 0b001110 2 =  binaryOp    (Rm ("cmp", 0))              0   d   w   mod     reg rm      regTable    read2
    | checkOp 0b001000 2 =  binaryOp    (Rm ("and", 0))              0   d   w   mod     reg rm      regTable    read2
    | checkOp 0b000010 2 =  binaryOp    (Rm ("or", 0))               0   d   w   mod     reg rm      regTable    read2
    | checkOp 0b001100 2 =  binaryOp    (Rm ("xor", 0))              0   d   w   mod     reg rm      regTable    read2
    | checkOp 0b0000010 1 = binaryOp    (Immediate [("add", 4)])     0   0   w   0b11    0   0b000   regTable    read1
    | checkOp 0b0001010 1 = binaryOp    (Immediate [("adc", 0)])     0   0   w   0b11    0   0b000   regTable    read1
    | checkOp 0b0010110 1 = binaryOp    (Immediate [("sub", 0)])     0   0   w   0b11    0   0b000   regTable    read1
    | checkOp 0b0001110 1 = binaryOp    (Immediate [("sbb", 0)])     0   0   w   0b11    0   0b000   regTable    read1
    | checkOp 0b0011110 1 = binaryOp    (Immediate [("cmp", 0)])     0   0   w   0b11    0   0b000   regTable    read1
    | checkOp 0b0010010 1 = binaryOp    (Immediate [("and", 0)])     0   0   w   0b11    0   0b000   regTable    read1
    | checkOp 0b0000110 1 = binaryOp    (Immediate [("or", 0)])      0   0   w   0b11    0   0b000   regTable    read1
    | checkOp 0b0011010 1 = binaryOp    (Immediate [("xor", 0)])     0   0   w   0b11    0   0b000   regTable    read1
    | checkOp 0b1000010 1 = binaryOp    (Rm ("test", 0))             0   0   w   mod     reg rm      regTable    read2
    | checkOp 0b1010100 1 = binaryOp    (Immediate [("test", 0)])    0   0   w   0b11    0   0b000   regTable    read1
    | checkOp 0b110100 2 =  binaryOp    (Logic logicTable)           d   0   w   mod     reg rm      regTable    read2
    | checkOp 0b10010 3 =   binaryOp    (Rm ("xchg", 0))             0   1   1   0b11    0   altreg  regTable    read1
    | checkOp 0b1110010 1 = binaryOp    (Immediate [("in", 0)])      0   0   0   0b11    0   (w * 0b1000)       regTable    read1
    | checkOp 0b1110110 1 = binaryOp    (Rm ("in", 0))               0   1   w   0b11    0   0b010   ioRegTable  read1
    | checkOp 0b1110011 1 = binaryOp    (Immediate [("out", 0)])     0   1   0   0b11    0   (w * 0b1000)       regTable    read1
    | checkOp 0b1110111 1 = binaryOp    (Rm ("out", 0))              0   0   w   0b11    0   0b010   ioRegTable  read1
    | checkOp 0b1111011 1 && getBits 3 3 (bytes U.! (iip + 1)) == 0b000 =
                            binaryOp    (Immediate [("test", 0)])    0   0   w   mod     reg rm      regTable    read2
    | checkOp 0b1011 4 =    binaryOp    (Immediate [("mov", 4)])     0   0   (getBits 1 3 (bytes U.! iip))
                                                                            0b11    0   altreg  regTable    read1

    | checkOp 0b1111011 1 = unaryOp     (Immediate unOpTable)        w   mod     reg rm      regTable    True    True    False   False read2
    | checkOp 0b10001111 0 =unaryOp     (Immediate [("pop", 0)])     1   mod     0   rm      regTable    True    True    False   False read2
    | checkOp 0b01010 3 =   unaryOp     (Rm ("push", 0))             1   0b11    0   altreg  regTable    True    True    False   False read1
    | checkOp 0b01011 3 =   unaryOp     (Rm ("pop", 0))              1   0b11    0   altreg  regTable    True    True    False   False read1
    | checkOp 0b01000 3 =   unaryOp     (Rm ("inc", 0))              1   0b11    0   altreg  regTable    True    True    False   False read1
    | checkOp 0b01001 3 =   unaryOp     (Rm ("dec", 0))              1   0b11    0   altreg  regTable    True    True    False   False read1
    | checkOp 0b11101001 0 =unaryOp     (Rm ("jmp", 0))              1   0b00    0   0b110   regTable    False   False   True    False read1
    | checkOp 0b11101000 0 =unaryOp     (Rm ("call", 0))             1   0b00    0   0b110   regTable    False   False   True    False read1
    | checkOp 0b11000010 0 =unaryOp     (Rm ("ret", 0))              1   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b11001010 0 =unaryOp     (Rm ("retf", 0))             1   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b01110100 0 =unaryOp     (Rm ("je", 0))               0   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b01111100 0 =unaryOp     (Rm ("jl", 0))               0   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b01111110 0 =unaryOp     (Rm ("jle", 0))              0   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b01110010 0 =unaryOp     (Rm ("jb", 0))               0   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b01110110 0 =unaryOp     (Rm ("jbe", 0))              0   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b01111010 0 =unaryOp     (Rm ("jp", 0))               0   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b01110000 0 =unaryOp     (Rm ("jo", 0))               0   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b01111000 0 =unaryOp     (Rm ("js", 0))               0   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b01110101 0 =unaryOp     (Rm ("jnz", 0))              0   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b01111101 0 =unaryOp     (Rm ("jge", 0))              0   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b01111111 0 =unaryOp     (Rm ("jg", 0))               0   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b01110011 0 =unaryOp     (Rm ("jae", 0))              0   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b01110111 0 =unaryOp     (Rm ("ja", 0))               0   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b01111011 0 =unaryOp     (Rm ("jnp", 0))              0   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b01110001 0 =unaryOp     (Rm ("jno", 0))              0   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b01111001 0 =unaryOp     (Rm ("jns", 0))              0   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b11100010 0 =unaryOp     (Rm ("loop", 0))             0   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b11100001 0 =unaryOp     (Rm ("loope", 0))            0   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b11100000 0 =unaryOp     (Rm ("loopnz", 0))           0   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b11100011 0 =unaryOp     (Rm ("jcxz", 0))             0   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b11001101 0 =unaryOp     (Rm ("int", 0))              0   0b00    0   0b110   regTable    False   False   False   False read1
    | checkOp 0b10011010 0 =unaryOp     (Rm ("call", 0))             1   0b00    0   0b110   regTable    False   False   False   True read1
    | checkOp 0b11101010 0 =unaryOp     (Rm ("jmp", 0))              1   0b00    0   0b110   regTable    False   False   False   True read1
    | checkOp 0b000 5 && getBits 3 0 (bytes U.! iip) == 0b110 =
                            unaryOp     (Rm ("push", 0))             0   0b11    0   sreg    segRegTable False   True    False   False read1
    | checkOp 0b000 5 && getBits 3 0 (bytes U.! iip) == 0b111 =
                            unaryOp     (Rm ("pop", 0))              0   0b11    0   sreg    segRegTable False   True    False   False read1
    | checkOp 0b1111111 1 = unaryOp     (Immediate stackTable)       w   mod     reg rm      regTable    (reg `elem` [0, 1, 6])
                                                                                                                 True    False   False read2

    | checkOp 0b11000011 0 = nullaryOp  ("ret", 0)    read1
    | checkOp 0b11001011 0 = nullaryOp  ("retf", 0)   read1
    | checkOp 0b11010111 0 = nullaryOp  ("xlat", 0)   read1
    | checkOp 0b10011111 0 = nullaryOp  ("lahf", 0)   read1
    | checkOp 0b10011110 0 = nullaryOp  ("sahf", 0)   read1
    | checkOp 0b10011100 0 = nullaryOp  ("pushf", 0)  read1
    | checkOp 0b10011101 0 = nullaryOp  ("popf", 0)   read1
    | checkOp 0b00110111 0 = nullaryOp  ("aaa", 0)    read1
    | checkOp 0b00100111 0 = nullaryOp  ("daa", 0)    read1
    | checkOp 0b00111111 0 = nullaryOp  ("aas", 0)    read1
    | checkOp 0b00101111 0 = nullaryOp  ("das", 0)    read1
    | checkOp 0b11010100 0 = nullaryOp  ("aam", 0)    read2
    | checkOp 0b11010101 0 = nullaryOp  ("aad", 0)    read2
    | checkOp 0b10011000 0 = nullaryOp  ("cbw", 0)    read1
    | checkOp 0b10011001 0 = nullaryOp  ("cwd", 0)    read1
    | checkOp 0b1111001 1 =  nullaryOp  ("rep", 0)    read1
    | checkOp 0b11001100 0 = nullaryOp  ("int3", 0)   read1
    | checkOp 0b11001110 0 = nullaryOp  ("into", 0)   read1
    | checkOp 0b11001111 0 = nullaryOp  ("iret", 0)   read1
    | checkOp 0b11111000 0 = nullaryOp  ("clc", 0)    read1
    | checkOp 0b11110101 0 = nullaryOp  ("cmc", 0)    read1
    | checkOp 0b11111001 0 = nullaryOp  ("stc", 0)    read1
    | checkOp 0b11111100 0 = nullaryOp  ("cld", 0)    read1
    | checkOp 0b11111101 0 = nullaryOp  ("std", 0)    read1
    | checkOp 0b11111010 0 = nullaryOp  ("cli", 0)    read1
    | checkOp 0b11111011 0 = nullaryOp  ("sti", 0)    read1
    | checkOp 0b11110100 0 = nullaryOp  ("hlt", 0)    read1
    | checkOp 0b10011011 0 = nullaryOp  ("wait", 0)   read1
    | checkOp 0b10100100 0 = nullaryOp  ("movsb", 0)  read1
    | checkOp 0b10100110 0 = nullaryOp  ("cmpsb", 0)  read1
    | checkOp 0b10101110 0 = nullaryOp  ("scasb", 0)  read1
    | checkOp 0b10101100 0 = nullaryOp  ("lodsb", 0)  read1
    | checkOp 0b10101010 0 = nullaryOp  ("stosb", 0)  read1
    | checkOp 0b10100101 0 = nullaryOp  ("movsw", 0)  read1
    | checkOp 0b10100111 0 = nullaryOp  ("cmpsw", 0)  read1
    | checkOp 0b10101111 0 = nullaryOp  ("scasw", 0)  read1
    | checkOp 0b10101101 0 = nullaryOp  ("lodsw", 0)  read1
    | checkOp 0b10101011 0 = nullaryOp  ("stosw", 0)  read1

    | checkOp 0b11110000 0 = set lock True read1
    | checkOp 0b001 5 && getBits 3 0 (bytes U.! iip) == 0b110 = set segmentOverride (Just $ fromIntegral sreg) read1

    | otherwise = error $ printf "%s\nunexepected opcode %08b" (foldr (\ a b -> a ++ "\n" ++ b) "" (_out state)) (bytes U.! iip)
    where
        bytes = _memory state
        iip = fromIntegral $ _ip state
        checkOp against shift = against == bytes U.! iip `shiftR` shift
        read2 = set ip (fromIntegral (iip + 2)) state
        read1 = set ip (fromIntegral (iip + 1)) state
        w = getBits 1 0 (bytes U.! iip)
        d = getBits 1 1 (bytes U.! iip)
        mod = getBits 2 6 (bytes U.! (iip + 1))
        reg = getBits 3 3 (bytes U.! (iip + 1))
        rm = getBits 3 0 (bytes U.! (iip + 1))
        altreg = getBits 3 0 (bytes U.! iip)
        sreg = getBits 2 3 (bytes U.! iip)

getBits :: Bits a => Int -> Int -> a -> a
getBits num shift byte = (byte `shiftL` (8 - (num + shift))) `shiftR` (8 - num)

segRegTable :: [[Char]]
segRegTable = [
    ""
    , ""
    , ""
    , ""
    , ""
    , ""
    , ""
    , ""
    , "es"
    , "cs"
    , "ss"
    , "ds"
    ]

regTable :: [String]
regTable = [
    "al"
    , "cl"
    , "dl"
    , "bl"
    , "ah"
    , "ch"
    , "dh"
    , "bh"
    , "ax"
    , "cx"
    , "dx"
    , "bx"
    , "sp"
    , "bp"
    , "si"
    , "di"
    ]

ioRegTable :: [String]
ioRegTable = [
    "al"
    , "cl"
    , "dx"
    , "bl"
    , "ah"
    , "ch"
    , "dh"
    , "bh"
    , "ax"
    , "cx"
    , "dx"
    , "bx"
    , "sp"
    , "bp"
    , "si"
    , "di"
    ]

requiresSizeSpecifier :: Optype -> Bool
requiresSizeSpecifier name = case name of
    (Rm _ ) -> False
    (Immediate _) -> True
    (Logic _) -> True

binaryOp :: Optype -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> [String] -> DisassemblerData -> DisassemblerData
binaryOp name s d w mod reg rm regs state = let
    sw = w - (if s == 1 && w == 1 then 1 else 0)
    (opName, clocks2) = case name of
        (Rm name) -> name
        (Immediate table) -> table !! fromIntegral reg
        (Logic table) -> table !! fromIntegral reg
    (arg1, clocks1, ip') = interpretrm w mod rm regTable state
    (arg2, ip'') = case name of
        (Rm _) -> (Register $ getReg regs w reg, ip')
        (Immediate _) -> (Value signExtended, ip' + (fromIntegral sw + 1))
            where
                bytes = _memory state
                initialVal = getDisplacement sw bytes ip'
                signExtended = if s == 1 then twosComplement 1 $ unTwosComplement 0 initialVal else initialVal
        (Logic _) -> (if s == 1 then Register "cl" else Value 1, ip')
    sizeSpecifier = if mod < 0b11 && requiresSizeSpecifier name
        then if w == 1 then "word " else "byte "
        else ""
    showArgs = (if d == 1 then reverse else id) [sizeSpecifier ++ show arg1, show arg2]

    args = (if d == 1 then reverse else id) [arg1, arg2]
    opFunc = opFromName opName
    in opFunc w args $  emitDisassemblerInstruction (clocks1 + clocks2) (opName ++ " " ++ head showArgs ++ ", " ++ showArgs !! 1, ip'') state

unaryOp :: Optype -> Word8 -> Word8 -> Word8 -> Word8 -> [String] -> Bool -> Bool -> Bool -> Bool -> DisassemblerData -> DisassemblerData
unaryOp name w mod reg rm regTable includeSize inBrackets addIp interSeg state = let
    opName = case name of
        (Rm (name, _)) -> name
        (Immediate table) -> fst $ table !! fromIntegral reg
        (Logic _) -> error "What"
    (arg, clocks, ip') = if inBrackets
        then interpretrm w mod rm regTable state
        else (Value $ getDisplacement w bytes ip + (if addIp then fromIntegral ip else 0)
             , 0
             , ip + (fromIntegral w + 1))
    (seg, ip'') = if interSeg then (show (getDisplacement w bytes ip') ++ ":", ip' + 2) else ("", ip')
    sizeSpecifier = if includeSize && mod < 0b11 && requiresSizeSpecifier name
        then if w == 1 then "word " else "byte "
        else ""
    opFunc = opFromName opName
    in opFunc w [arg] $ emitDisassemblerInstruction clocks (opName ++ " " ++ sizeSpecifier ++ seg ++ show arg, ip'') state
    where
        bytes = _memory state
        ip = _ip state

nullaryOp :: (String, Int)-> DisassemblerData -> DisassemblerData
nullaryOp (name, clocks) state = emitDisassemblerInstruction clocks (name, _ip state) state


getReg :: [String] -> Word8 -> Word8 -> String
getReg regTable w reg = regTable !! fromIntegral (w * 0b1000 + reg)

getDisplacement :: Word8 -> Memory -> Word16 -> Int
getDisplacement w bytes ip = if w == 0 then byte1 else fromIntegral (bytes U.! (fromIntegral ip + 1)) * 256 + byte1
    where byte1 = fromIntegral $ bytes U.! fromIntegral ip

unTwosComplement :: Word8 -> Int -> Int
unTwosComplement w num = if num >= max
    then -(complement (num - 1) .&. (max - 1))
    else num
    where
        max = if w == 0 then 128 else 32768

twosComplement :: Word8 -> Int -> Int
twosComplement w num = if num < 0 then ((abs num `xor` full) + 1) .&. full else num
    where
        full = if w == 0 then 255 else 65535

interpretrm :: Word8 -> Word8 -> Word8 -> [String] -> DisassemblerData -> (Argtype, Int, Word16)
interpretrm w mod rm regTable state = if mod == 0b11
    then (Register $ getReg regTable w rm, 0, ip)
    else (Mem Addressifier {
            displacement=if mod > 0 || specialCased then displacement else 0
            , eqType=if specialCased then -1 else fromIntegral rm
            , segReg=case _segmentOverride state of
                (Just override) -> Just $ segRegTable !! (fromIntegral override + 8)
                Nothing -> Nothing
            , segOffset=0
            }
        , (tracer $ if specialCased then 6
          else if mod == 0 && rm >= 4 then 5
          else if mod > 0 && rm >= 4 then 9
          else if mod == 0 && rm == 0 || rm == 3 then 7
          else if mod == 0 && rm == 1 || rm == 2 then 8
          else if mod > 0 && rm == 0 || rm == 3 then 11
          else if mod > 0 && rm == 1 || rm == 2 then 12
          else error $ "huh" ++ show rm ++ show mod)
          + (if isJust $ _segmentOverride state then 2 else 0)
        , if specialCased then ip + 2
          else if mod > 0 then ip + fromIntegral mod
          else ip)
        where
            bytes = _memory state
            ip = _ip state
            displacement = getDisplacement (mod - 1) bytes ip
            specialCased = mod == 0b00 && rm == 0b110
