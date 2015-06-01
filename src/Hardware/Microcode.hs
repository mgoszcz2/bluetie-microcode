{-# LANGUAGE DeriveGeneric #-} -- REMOVE
{-# LANGUAGE NamedFieldPuns #-} -- REMOVE
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module Hardware.Microcode where

import Control.Monad.Writer
import Data.Enumerable.Generic
import Data.List ((\\))
import Data.Ord (comparing)
import GHC.Generics (Generic)
import Data.Label (mkLabel, set)
import Control.Arrow ((***), first, second)
import Data.List (mapAccumL, mapAccumR, sortBy)
import qualified Data.ByteString as BS

type Signals f s = Writer [[(f, s)]] ()
type Directive i = Writer (Endo i) ()
type Bits = [(String, [Bit])]

data Bit = Low | High deriving (Show, Eq)
data States f i = States Int f i deriving (Show)
data AddressRange = Flags Int | State Int | Instruction Int deriving (Show)
data RomConfig = RomConfig { romCount :: Int
                           , romAddressSize :: Int
                           , addressRange :: [AddressRange]
                           } deriving (Show)

instance Enumerable Bit where
    per Low = (High, False)
    per High = (Low, True)

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
flattenRom :: (Signal s, Flags f, Instruction i) => i -> [[(f, s)]] -> [(States f i, s)]
flattenRom i = snd . foldl countTicks (0, [])
    where countTicks (ac, flat) perflag = (ac + 1, flat ++ map (makeTuple ac) perflag)
          makeTuple ac (f, s) = (States ac f i, s)

-- | Given a "bit map" and `States` produce a `Bits` structure
stateBits :: (Instruction i, Flags f) => [AddressRange] -> States f i -> Bits
stateBits ar (States s f i) = concatMap truncated ar
    where truncated x = bitsTruncate (fromRange x) $ case x of
                            State a -> bitInt "state" a s
                            Flags _ -> flagBits f
                            Instruction _ -> instructonBits i

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
        => RomConfig -> (i -> Signals f s) -> [i] -> [[Bit]]
makeRom conf@RomConfig{..} dsl ins
    | realCnt /= romAddressSize = error $ "Invalid bit counts: " ++ show realCnt
    | otherwise = bitsRom conf $ concatMap runDsl ins
    where runDsl i = flattenRom i . execWriter $ dsl i
          realCnt = addressRangeSize addressRange

-- | Turn all `States` and `Signal`s into sorted output values
bitsRom :: (Instruction i, Flags f, Signal s)
        => RomConfig -> [(States f i, s)] -> [[Bit]]
bitsRom RomConfig{..} sts = map snd . sortWith fst $ bits ++ map ((`first` filler) . const) missingIns
    where missingIns = [0..2 ^ addressRangeSize addressRange - 1] \\ (map fst bits)
          bits = map (bitsToInt . stateBits addressRange *** concatMap snd . signalBits) sts
          filler = (0, concatMap snd . signalBits $ nop `asTypeOf` (snd $ head sts))

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
bit0 = makeBits "Low" [Low]
bit1 = makeBits "High" [High]

intToBit :: Int ->  [Bit]
intToBit 0 = []
intToBit n = (if n `mod` 2 == 1 then High else Low) : intToBit (n `div` 2)

bitToInt :: [Bit] -> Int
bitToInt = sum . zipWith (*) (iterate (*2) 1) . map biti
    where biti Low = 0
          biti High = 1

bitInt :: String -> Int -> Int -> Bits
bitInt doc bits num = makeBits doc . take bits $ intToBit num ++ repeat Low

bitsToInt :: Bits -> Int
bitsToInt = bitToInt . concatMap snd

-- THE UGLY PARTS FOR TESTING --
data TieFlags = TieFlags { carry, equal, zero, sign, icarry, int :: Bit }
                         deriving (Show, Generic)

data TieSignal = TieSignal { _writeReg :: Bit
                           , _halted :: Bit
                           , _busOne :: Bit
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
    def = TieFlags Low Low Low Low Low Low
instance Enumerable TieInstruction
instance Instruction TieInstruction where
    instructonBits Halt = makeBits "instruction" [Low, Low]
    instructonBits Move = makeBits "instruction" [Low, High]
    instructonBits Add = makeBits "instruction" [High, Low]
instance Defaults TieInstruction where
    defs = [Halt, Move, Add]
instance Signal TieSignal where
    nop = TieSignal Low Low Low
    signalBits TieSignal{..} = bit "writeReg" _writeReg ++ bit "halted" _halted ++ bit "busOne" _busOne

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

testMakeRom = makeRom sampleConf process defs

sampleConf :: RomConfig
sampleConf = RomConfig 1 10 [Flags 6, State 2, Instruction 2]
