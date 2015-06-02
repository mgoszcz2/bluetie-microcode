{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DefaultSignatures #-}
module Hardware.Microcode (
    -- * Modules
    module Data.Enumerable.Generic
    -- * Common functions
    , makeRom
    , exportAsFile
    , exportAsLogisim
    -- ** `Bits` construction
    , bit0
    , bit1
    , bitInt
    , bit
    -- ** `Signals` construction functions
    , nothing
    , tick
    , tickF
    -- ** Other functions
    , makeBits
    , bitDocument
    , cmd
    -- * Types
    , Signals
    , Directive
    , Bits
    , Bit(..)
    , AddressRange(..)
    , RomConfig(..)
    -- * Classes
    , Signal (nop, signalBits)
    , Flags (flags, flagBits)
    , Instruction (instructions, instructonBits)
    ) where

import Data.Monoid
import Data.Enumerable.Generic
import Data.Word (Word8)
import Text.Printf (printf)
import Numeric (showHex)
import Data.Ord (comparing)
import Control.Arrow ((***), first)
import Data.List (mapAccumL, sortBy, transpose, (\\))
import Control.Monad.Writer (execWriter, tell, Writer)
import qualified Data.ByteString as BS

-- Harmful do notation strikes again.. but it looks nicer
type Signals f s = Writer [[(f, s)]] ()
type Directive s = Writer (Endo s) ()
type Bits = [(String, [Bit])]

-- | Eletronics equivalent of `Bool`
data Bit = Low | High deriving (Show, Eq)
data States f i = States Int f i deriving (Show)
-- | Types of inputs of a ROM. Repeated enteries are allowed but kind of pointless
data AddressRange = Flags Int | State Int | Instruction Int deriving (Show)
-- | Configuration data for `makeRom` functin
data RomConfig = RomConfig { romCount :: Int -- ^ Amount of 8-bit ROM chips aveliable
                           , romAddressSize :: Int -- ^ Number of input bits in each ROM.. Should add up to `addressRange`
                           , addressRange :: [AddressRange] -- ^ Order of inputs into a ROM
                           } deriving (Show)

instance Enumerable Bit where
    per Low = (High, False)
    per High = (Low, True)

class Signal s where
    -- | Default value for a signal that does nothing
    nop :: s
    default nop :: (Default s) => s
    nop = def
    -- | Serialize a signal into `Bits`
    signalBits :: s -> Bits

class Flags f where
    -- | Return all possible variations of flags
    flags :: [f]
    default flags :: (Default f, Enumerable f) => [f]
    flags = allEnum
    -- | Serialize flag value into `Bits`
    flagBits :: f -> Bits

class Instruction i where
    -- | Assemble an instruction into `Bits`
    instructonBits :: i -> Bits
    -- | Returns all possible valid instructions
    instructions :: [i]
    default instructions :: (Defaults i, Enumerable i) => [i]
    instructions = allDefsEnum

-- | Dcoumentation helper function. Turn `Bits` into a string and range pairs
bitDocument :: Bits -> [(String, (Int, Int))]
bitDocument = snd . mapAccumL makeDoc 0
    where makeDoc a (doc, bs) = let na = a + length bs
                                in (na, (doc, (a, na)))

-- | Run the `Endo` and `Writer` using the `nop` instruction for inital value
makeDirective :: (Signal i) => Directive i -> i
makeDirective c = (appEndo $ execWriter c) nop

-- | Tick, calling a `Directive` function with current flags
tickF :: (Signal i, Flags f) => (f -> Directive i) -> Signals f i
tickF fn = tell . return $ map pair flags
    where pair f = (f, makeDirective $ fn f)

-- | Just tick, no conditionals
tick :: (Signal i, Flags f) => Directive i -> Signals f i
tick = tell . return . zip flags . repeat . makeDirective

-- | Simplest `Directive`. Makes no modfications to the signals
nothing :: (Signal s) => Directive s
nothing = cmd id

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

-- | Simple checks and DSL execution
makeRom :: (Instruction i, Flags f, Signal s)
        => RomConfig -> (i -> Signals f s) -> [[Word8]]
makeRom conf@RomConfig{..} dsl
    | realCnt /= romAddressSize = error $ "Invalid bit counts: " ++ show realCnt
    | length rom /= romCount = error $ "Rom count mismatch: " ++ show (length rom)
    | otherwise = rom
    where runDsl i = flattenRom i . execWriter $ dsl i
          realCnt = addressRangeSize addressRange
          rom = map bitsExport . prepRomBits conf $ concatMap runDsl instructions

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

-- | Fancy way of putting things in a single list.. but more abstract and compatible
makeBits :: String -> [Bit] -> Bits
makeBits doc ds = return (doc, ds)

-- | Helper for simply returning `Bit`s
bit :: String -> Bit -> Bits
bit doc pre = makeBits doc [pre]

-- | Helpers for bit constants
bit0, bit1 :: Bits
bit0 = makeBits "low" [Low]
bit1 = makeBits "high" [High]

-- | Turn an `Int` into a series of `Bit`s
intToBit :: Int ->  [Bit]
intToBit 0 = []
intToBit n = (if n `mod` 2 == 1 then High else Low) : intToBit (n `div` 2)

-- | Turns binary `Bit`s into an `Int`
bitToInt :: [Bit] -> Int
bitToInt = sum . zipWith (*) (iterate (*2) 1) . map biti
    where biti Low = 0
          biti High = 1

-- | Helper for creating `Bits` for `Int`s, doc followed by bits to take
bitInt :: String -> Int -> Int -> Bits
bitInt doc bits num = makeBits doc . take bits $ intToBit num ++ repeat Low

-- | Turns a `Bits` structure into an `Int`
bitsToInt :: Bits -> Int
bitsToInt = bitToInt . concatMap snd

-- | DSL command creating helper
cmd :: (Signal i) => (i -> i) -> Directive i
cmd = tell . Endo

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

