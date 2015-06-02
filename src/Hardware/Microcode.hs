{-# LANGUAGE DeriveGeneric #-} -- REMOVE
{-# LANGUAGE NamedFieldPuns #-} -- REMOVE
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module Hardware.Microcode where

import Data.Monoid
import Data.Enumerable.Generic
import Data.Word (Word8)
import Data.Generics.Is (makePredicates)
import Text.Printf (printf)
import Numeric (showHex)
import Data.Ord (comparing)
import GHC.Generics (Generic)
import Data.Label (mkLabel, set)
import Control.Arrow ((***), first)
import Data.List (mapAccumL, sortBy, transpose, (\\))
import Control.Monad.Writer (execWriter, tell, Writer)
import qualified Data.ByteString as BS

-- Harmful do notation strikes again.. but it looks nicer
type Signals f s = Writer [[(f, s)]] ()
type Bits = [(String, [Bit])]

data Bit = Low | High deriving (Show, Eq)
data States f i = States Int f i deriving (Show)
data AddressRange = Flags Int | State Int | Instruction Int deriving (Show)
data RomConfig = RomConfig { romCount :: Int
                           , romAddressSize :: Int
                           , addressRange :: [AddressRange]
                           } deriving (Show)

$(makePredicates ''AddressRange)

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
    default instructions :: (Defaults i, Enumerable i) => [i]
    instructions :: [i]
    instructions = allDefsEnum

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
tickF :: (Signal i, Flags f) => (f -> Endo i) -> Signals f i
tickF fn = tell . return $ map pair flags
    where pair f = (f, fn f `appEndo` nop)

-- | Just tick, no conditionals
tick :: (Signal i, Flags f) => Endo i -> Signals f i
tick = tell . return . zip flags . repeat . (`appEndo` nop)

-- | Simplest `Directive`. Makes no modfications to the signals
nothing :: (Signal i) => Endo i
nothing = mempty

-- | Turn a nested per tick/per flag strucure into a flat one
flattenRom :: (Signal s, Flags f, Instruction i) => i -> [[(f, s)]] -> [(States f i, s)]
flattenRom i = snd . foldl countTicks (0, [])
    where countTicks (ac, flat) perflag = (ac + 1, flat ++ map (makeTuple ac) perflag)
          makeTuple ac (f, s) = (States ac f i, s)

-- | Given a "bit map" and `States` produce a `Bits` structure
stateBits :: (Instruction i, Flags f) => [AddressRange] -> States f i -> Bits
stateBits ar (States s f i) = concatMap truncated ar
    where truncated (State a) = bitInt "state" a s
          truncated (Flags _) = flagBits f
          truncated (Instruction _) = instructonBits i

-- How many bits are needs for all input data
addressRangeSize :: [AddressRange] -> Int
addressRangeSize = sum . map fromRange

-- Total number of bits in `Bits`.. Not as simple as it sounds
bitsLength :: Bits -> Int
bitsLength = foldr ((+) . length . snd) 0

-- `GHC.Exts`'s `sortWith` but using standard Haskell
sortWith :: (Ord b) => (a -> b) -> [a] -> [a]
sortWith = sortBy . comparing

-- | Split a list so that a sub-list is never longer than N
chunk :: Int -> [a] -> [[a]]
chunk _ []    = []
chunk r xs    = take r xs : chunk r (drop r xs)

-- | Finnal step, turn `Bit` lists into `Word8`
bitsExport :: [[Bit]] -> [Word8]
bitsExport = map (fromIntegral . bitToInt)

-- | Simple checks and DSL execution. For book-keeping see `prepRomBits`
makeRom :: (Instruction i, Flags f, Signal s)
        => RomConfig -> (i -> Signals f s) -> [[Word8]]
makeRom conf@RomConfig{..} dsl
    | realCnt /= romAddressSize = error $ "Invalid bit counts: " ++ show realCnt
    | otherwise = map bitsExport . prepRomBits conf $ concatMap runDsl instructions
    where runDsl i = flattenRom i . execWriter $ dsl i
          realCnt = addressRangeSize addressRange

-- | Turn all `States` and `Signal`s into sorted output values
-- Splits outputs into 8-bit chunks, fills in any missing gaps with `nop`s
-- And discrads input data by sorting. In short: General book-keeping
prepRomBits :: (Instruction i, Flags f, Signal s)
            => RomConfig -> [(States f i, s)] -> [[[Bit]]]
prepRomBits RomConfig{..} sts = transpose $ map (chunk 8 . snd) newSts
    where missingIns = [0..2 ^ addressRangeSize addressRange - 1] \\ (map fst bits)
          bits = map (bitsToInt . stateBits addressRange *** concatMap snd . signalBits) sts
          filler = (0 :: Int, concatMap snd . signalBits $ nop `asTypeOf` (snd $ head sts)) -- Uh.. Oh
          newSts = sortWith fst $ bits ++ map ((`first` filler) . const) missingIns

-- | All `AddressRange` contain an `Int` signifing the size in bits. Extract it
fromRange :: AddressRange -> Int
fromRange (Flags i) = i
fromRange (State i) = i
fromRange (Instruction i) = i

-- | Simply extract `State` from `AddressRange`s and calculate as power of 2
getStateCount :: [AddressRange] -> Int
getStateCount = (2 ^) . fromRange . head . filter isState -- This is supposed to be bad

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

-- | Helper function that helps with writing other export helpers
exportHelper :: (String -> [Word8] -> IO ()) -> String -> [[Word8]] -> IO ()
exportHelper io filen rom = mapM_ wrapIo $ zip ([0..] :: [Int]) rom
    where wrapIo (ix, bs) = do let fullname = show ix ++ filen
                               io fullname bs
                               printf "Wrote ROM #%d to file '%s'\n" ix fullname

-- | Helper to export the `makeRom` output into binary file(s)
exportAsFile :: String -> [[Word8]] -> IO ()
exportAsFile = exportHelper (\n d -> BS.writeFile n $ BS.pack d)

-- | Helper to export `makeRom` output into Logisim format
exportAsLogisim :: String -> [[Word8]] -> IO ()
exportAsLogisim = exportHelper write
    where write filen xs = writeFile filen . unlines $ "v2.0 raw" : map (flip showHex "") xs
                              

-- THE UGLY PARTS FOR TESTING --
data TieFlags = TieFlags { carry, equal, zero, sign, icarry, int :: Bit }
                         deriving (Show, Generic)

data TieSignal = TieSignal { _writeReg :: Bit
                           , _halted :: Bit
                           , _busOne :: Bit
                           , _busTwo :: Bit
                           , _butThree :: Bit
                           , _busFour :: Bit
                           , _rstState :: Bit
                           , _rstFlags :: Bit
                           , _wrtFlags :: Bit
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
instance Default TieFlags where
    def = TieFlags Low Low Low Low Low Low
instance Enumerable TieFlags
instance Instruction TieInstruction where
    instructonBits Halt = makeBits "instruction" [Low, Low]
    instructonBits Move = makeBits "instruction" [Low, High]
    instructonBits Add = makeBits "instruction" [High, Low]
instance Defaults TieInstruction where
    defs = [Halt, Move, Add]
instance Enumerable TieInstruction
instance Signal TieSignal where
    nop = TieSignal Low Low Low Low Low Low Low Low Low
    signalBits TieSignal{..} = bit "writeReg" _writeReg ++
                               bit "halted" _halted ++
                               bit "busOne" _busOne ++
                               bit "busTwo" _busTwo ++
                               bit "butThree" _butThree ++
                               bit "busFour" _busFour ++
                               bit "rstState" _rstState ++
                               bit "rstFlags" _rstFlags ++
                               bit "wrtFlags" _wrtFlags

showNested :: (Show a) => [[a]] -> String
showNested = unlines . map (unlines . map show)

regWrite :: Bit -> Endo TieSignal
regWrite d = Endo $ set writeReg d

process :: TieInstruction -> Signals TieFlags TieSignal
process Halt = do
    tickF $ \TieFlags{carry} -> nothing <> regWrite carry
    tick nothing
process _ = tick nothing

sampleConf :: RomConfig
sampleConf = RomConfig 2 10 [Flags 6, State 2, Instruction 2]
