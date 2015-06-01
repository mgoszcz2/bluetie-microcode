{-# LANGUAGE DeriveGeneric #-} -- REMOVE
{-# LANGUAGE NamedFieldPuns #-} -- REMOVE
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module Hardware.Microcode where

import Data.Label
import Data.Monoid
import Control.Monad.Writer
import Data.Enumerable.Generic
import Data.Ord (comparing)
import GHC.Generics (Generic)
import Data.List (mapAccumL, mapAccumR, sortBy)
import qualified Data.ByteString as BS

type Signals f s = Writer [[(f, s)]] () -- Accumulated ticks
type Directive i = Writer (Endo i) () -- Acumulated microassembly
type Bits = [(String, [Bit])] -- Serialised bits
type States f i = (Int, f, i) -- Cpu state
type Bit = Bool

data AddressRange = Flags Int | State Int | Instruction Int deriving (Show)

data RomConfig = RomConfig { romCount :: Int
                           , romAddressSize :: Int
                           , addressRange :: [AddressRange]
                           } deriving (Show)

class Signal s where
    nop :: s
    default nop :: (Default s) => s
    nop = def
    signalBits :: s -> Bits

class Flags f where
    flags :: [f]
    default flags :: (Default f, Enumerable f) => [f]
    flags = allEnum
    flagBits :: f -> Bits

class Instruction i where
    instructonBits :: i -> Bits

-- | Given an `AddressRange` extract the length `Int`
fromAddressRange :: AddressRange -> Int
fromAddressRange (Flags a) = a
fromAddressRange (State a) = a
fromAddressRange (Instruction a) = a

-- | Dcoumentation helper function. Turn `Bits` into a string and range pairs
bitDocument :: Bits -> [(String, (Int, Int))]
bitDocument = snd . mapAccumL makeDoc 0
    where makeDoc a (doc, bs) = let na = a + length bs
                                in (na, (doc, (a, na)))

-- | Tick, calling a `Directive` function with current flags
tickF :: (Signal i, Flags f) => (f -> Directive i) -> Signals f i
tickF fn = tell . return $ map pair flags
    where pair f = (f, makeDirective $ fn f)

-- | Just tick, no conditionals
tick :: (Signal i, Flags f) => Directive i -> Signals f i
tick = tell . return . zip flags . repeat . makeDirective

-- | Run the `Endo` and `Writer` using the `nop` instruction for inital value
makeDirective :: (Signal i) => Directive i -> i
makeDirective c = (appEndo $ execWriter c) nop

-- | Quick sugar for writing `Directive` commands
cmd :: (Signal i) => (i -> i) -> Directive i
cmd = tell . Endo

-- | Simplest `Directive`. Makes no modfications to the signals
nothing :: (Signal i) => Directive i
nothing = cmd id

-- | Turn a nested per tick/per flag strucure into a flat one
flattenRom :: (Instruction i, Flags f, Signal s) => i -> [[(f, s)]] -> [(States f i, s)]
flattenRom i = snd . foldl countTicks (0, [])
    where countTicks (ac, flat) perflag = (ac + 1, flat ++ map (makeTuple ac) perflag)
          makeTuple ac (f, s) = ((ac, f, i), s)

-- | Pad a nested raw states strucutre to correct number of states, if they are missing
padStates :: (Flags f, Signal s) => Int -> [[(f, s)]] -> [[(f, s)]]
padStates sts cnt = take sts $ cnt ++ repeat blank
    where blank = map (\f -> (f, nop)) flags

stateBits :: (Instruction i, Flags f) => [AddressRange] -> States f i -> Bits
stateBits ar (s, f, i) = concatMap truncated ar
     where getBit (State a) = bitInt "state" a s
           getBit (Flags _) = flagBits f
           getBit (Instruction _) = instructonBits i
           truncated x = bitsTruncate (fromRange x) (getBit x)

addressRangeSize :: [AddressRange] -> Int
addressRangeSize = sum . map fromRange

bitsLength :: Bits -> Int
bitsLength = foldr ((+) . length . snd) 0

sortWith :: (Ord b) => (a -> b) -> [a] -> [a]
sortWith = sortBy . comparing

bitsTruncate :: Int -> Bits -> Bits
bitsTruncate wide = snd . mapAccumR truncateAcc 0
    where truncateAcc a (d, b) = (a + length b, (d, take (wide - a) b))

makeRom :: (Instruction i, Flags f, Signal s)
        => RomConfig -> (i -> Signals f s) -> [i] -> [(Int, Int)]
makeRom RomConfig{..} dsl ins
    | apparentCount /= romAddressSize = error $ "Invalid bit counts: " ++ show apparentCount
    | otherwise = bitsRom addressRange $ concatMap runDsl ins
    where apparentCount = sum $ map fromAddressRange addressRange
          runDsl i = flattenRom i . padStates (getStateCount addressRange) . execWriter $ dsl i

bitsRom :: (Instruction i, Flags f, Signal s)
        => [AddressRange] -> [(States f i, s)] -> [(Int, Int)]
bitsRom addrRng = sortWith fst . map genBits
    where genBits (t, i) = (bitsToInt $ stateBits addrRng t, bitsToInt $ signalBits i)

fromRange :: AddressRange -> Int
fromRange (Flags i) = i
fromRange (State i) = i
fromRange (Instruction i) = i

getStateCount :: [AddressRange] -> Int
getStateCount = (2 ^) . fromRange . head . filter scnt -- This is supposed to be bad
    where scnt (State _) = True
          scnt _         = False

lastN :: Int -> [a] -> [a]
lastN n xs = foldl (const . drop 1) xs $ drop n xs

makeBits :: String -> [Bit] -> Bits
makeBits doc ds = return (doc, ds)

bit :: String -> Bit -> Bits
bit doc pre = makeBits doc [pre]

bit0, bit1 :: Bits
bit0 = makeBits "Low" [False]
bit1 = makeBits "High" [True]

intToBit :: Int ->  [Bit]
intToBit 0 = []
intToBit n = (n `mod` 2 == 1) : intToBit (n `div` 2)

boolInt :: Bool -> Int
boolInt True = 1
boolInt False = 0

bitToInt :: [Bit] -> Int
bitToInt = sum . zipWith (*) (iterate (*2) 1) . map boolInt

bitInt :: String -> Int -> Int -> Bits
bitInt doc bits num = makeBits doc . take bits $ intToBit num ++ repeat False

bitsToInt :: Bits -> Int
bitsToInt = bitToInt . concatMap snd

-- THE UGLY PARTS FOR TESTING --
data TieFlags = TieFlags { carry, equal, zero, sign, icarry, int :: Bit }
                         deriving (Show, Generic)

data TieSignal = TieSignal { _writeReg :: Bit
                           , _halted :: Bit
                           } deriving (Show)

data TieInstruction = Halt | Move | Add deriving (Eq, Show, Generic)

mkLabel ''TieSignal

instance Flags TieFlags where
    flagBits TieFlags{..} = bit "carry" carry ++
                            bit "equal" equal ++
                            bit "zero" zero ++
                            bit "sign" sign ++
                            bit "icarry" icarry ++
                            bit "interrupt" int
instance Enumerable TieFlags
instance Default TieFlags where
    def = TieFlags False False False False False False
instance Enumerable TieInstruction
instance Instruction TieInstruction where
    instructonBits Halt = makeBits "instruction" [False, False]
    instructonBits Move = makeBits "instruction" [False, True]
    instructonBits Add = makeBits "instruction" [True, False]
instance Defaults TieInstruction where
    defs = [Halt, Move, Add]
instance Signal TieSignal where
    nop = TieSignal False False
    signalBits TieSignal{..} = bit "writeReg" _writeReg ++ bit "halted" _halted

showNested :: (Show a) => [[a]] -> String
showNested = unlines . map (unlines . map show)

regWrite :: Bit -> Directive TieSignal
regWrite d = cmd $ set writeReg d

process :: TieInstruction -> Signals TieFlags TieSignal
process Halt = do
    tickF $ \TieFlags{carry} -> do nothing
                                   regWrite carry
    tick nothing
process _ = tick nothing

testMakeRom = makeRom sampleConf process allDefsEnum

sampleConf :: RomConfig
sampleConf = RomConfig 1 10 [Flags 6, State 2, Instruction 2]
