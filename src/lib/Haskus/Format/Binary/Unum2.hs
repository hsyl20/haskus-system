{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

-- | Universal numbers
--
-- TODO: compressed connected SORNs
module Haskus.Format.Binary.Unum2
   ( Unum (..)
   , UnumWord
   , unumInfinity
   , unumZero
   , unumNegate
   , unumReciprocate
   , unumIsOpenInterval
   , unumIsExactNumber
   , unumBitCount
   , toUnum
   , fromUnum
   , rcp
   , unumMask
   , unumNext
   , unumPrev
   , unumLowerBound
   , unumUpperBound
   , Sign (..)
   , unumSign
   -- * Set of real numbers
   , UnumSet (..)
   , SORN (..)
   , SORNSize
   , SORNWord
   , sornMask
   , sornElems
   , sornEmpty
   , sornFull
   , sornInsert
   , sornRemove
   , sornFromList
   , sornUnions
   , sornUnion
   -- * Operations
   , unumAdd
   , unumMul
   -- * Numeric systems
   , Unum2b (..)
   , Unum3b (..)
   )
where

import Data.Set as Set
import GHC.Real
--import qualified Data.Vector as V

import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Bits
import Haskus.Utils.Types
import Haskus.Utils.Flow
import Haskus.Utils.List as List

type family UnumWord x where
   UnumWord x = WordAtLeast (UnumBitCount x)

-- | Numeric system
class Eq x => Unum x where
   -- | Number of bits to store a number
   type UnumBitCount x :: Nat

   -- | Pack a number
   unumPack :: UnumWord x -> x

   -- | Unpack a number
   unumUnpack :: x -> UnumWord x

   -- Strictly positive exact members of the numeric system without their reciprocals
   --
   -- We then include the inverse and the reciprocal of every number (0
   -- and its reciprocal are included too)
   unumInputMembers :: x -> Set Rational

   -- | All the exact members of the numeric system (including `infinity`)
   unumExactMembers :: x -> Set Rational
   unumExactMembers x = Set.unions
      [ unumPositiveMembers x
      , unumNegativeMembers x
      , Set.singleton 0
      , Set.singleton infinity
      ]

   -- | Positive members
   unumPositiveMembers :: x -> Set Rational
   unumPositiveMembers x = Set.unions
      [ unumInputMembers x
      , Set.map rcp (unumInputMembers x)
      ]

   -- | Negative members
   unumNegativeMembers :: x -> Set Rational
   unumNegativeMembers = Set.map (0 -) . unumPositiveMembers


-- | Mask a Unum Word
unumMask :: forall u.
   ( Unum u
   , FiniteBits (UnumWord u)
   , KnownNat (UnumBitCount u)
   ) => UnumWord u -> UnumWord u
unumMask = maskLeastBits (unumBitCount @u)

-- | Next unum
unumNext :: forall u.
   ( Unum u
   , FiniteBits (UnumWord u)
   , KnownNat (UnumBitCount u)
   , Num (UnumWord u)
   ) => u -> u
unumNext = unumPack . unumMask @u . (+1) . unumUnpack

-- | Previous unum
unumPrev :: forall u.
   ( Unum u
   , FiniteBits (UnumWord u)
   , KnownNat (UnumBitCount u)
   , Num (UnumWord u)
   ) => u -> u
unumPrev = unumPack . unumMask @u . (\x -> x-1) . unumUnpack

-- | Exact lower bound
unumLowerBound ::
   ( Unum u
   , FiniteBits (UnumWord u)
   , Num (UnumWord u)
   , KnownNat (UnumBitCount u)
   ) => u -> u
unumLowerBound = unumPack . (`clearBit` 0) . unumUnpack

-- | Exact upper bound
unumUpperBound ::
   ( Unum u
   , FiniteBits (UnumWord u)
   , Num (UnumWord u)
   , KnownNat (UnumBitCount u)
   ) => u -> u
unumUpperBound u
   | unumIsExactNumber u = u
   | otherwise           = unumNext u

-- | Reciprocate
rcp :: Rational -> Rational
rcp (n :% d) = d :% n

unumIsOpenInterval :: 
   ( FiniteBits (UnumWord u)
   , Unum u
   ) => u -> Bool
{-# INLINE unumIsOpenInterval #-}
unumIsOpenInterval u = testBit (unumUnpack u) 0

unumIsExactNumber ::
   ( FiniteBits (UnumWord u)
   , Unum u
   ) => u -> Bool
{-# INLINE unumIsExactNumber #-}
unumIsExactNumber = not . unumIsOpenInterval

unumBitCount :: forall u. (KnownNat (UnumBitCount u)) => Word
unumBitCount = natValue @(UnumBitCount u)

-- | Negate a number
unumNegate :: forall u.
   ( FiniteBits (UnumWord u)
   , Num (UnumWord u)
   , KnownNat (UnumBitCount u)
   , Unum u
   ) => u -> u
{-# INLINE unumNegate #-}
unumNegate u =
   unumPack
   <| maskLeastBits (unumBitCount @u)
   <| complement (unumUnpack u) + 1


data Sign
   = Negative
   | Positive
   | NoSign
   deriving (Show,Eq,Ord)

-- | Get unum sign
unumSign :: forall u.
   ( Unum u
   , KnownNat (UnumBitCount u)
   , Eq (UnumWord u)
   , Num (UnumWord u)
   , FiniteBits (UnumWord u)
   ) => u -> Sign
unumSign u = if n == 0
      then NoSign
      else if s
         then Negative
         else Positive
   where
      w = unumUnpack u
      s = testBit w (fromIntegral (unumBitCount @u - 1))
      n = clearBit w (fromIntegral (unumBitCount @u - 1))

-- | Unum infinity
unumInfinity :: forall u.
   ( Unum u
   , FiniteBits (UnumWord u)
   , Num (UnumWord u)
   , KnownNat (UnumBitCount u)
   ) => u
unumInfinity = unumPack (bit (fromIntegral (unumBitCount @u - 1)))

-- | Unum zero
unumZero ::
   ( Unum u
   , FiniteBits (UnumWord u)
   ) => u
unumZero = unumPack zeroBits


-- | Reciprocate a number
unumReciprocate :: forall u.
   ( FiniteBits (UnumWord u)
   , Num (UnumWord u)
   , KnownNat (UnumBitCount u)
   , Unum u
   ) => u -> u
{-# INLINE unumReciprocate #-}
unumReciprocate u =
   unumPack
   <| (unumUnpack u `xor` m + 1)
   where
      s = unumBitCount @u
      m = makeMask (s-1)

-- | Build a number from a Rational
toUnum :: forall u.
   ( Unum u
   , FiniteBits (UnumWord u)
   , Num (UnumWord u)
   , KnownNat (UnumBitCount u)
   ) => Rational -> u
toUnum x
   | x == infinity = unumInfinity
   | x < 0         = unumNegate      (toUnum (0 - x))
   | x == 0        = unumPack 0
   | x < 1         = unumReciprocate (toUnum (rcp x))
   | otherwise     = case go 0 x es of
         (b,certain) -> unumPack ((b `shiftL` 1) .|. if certain then 0 else 1)
      where
         es = 0 : Set.toAscList (unumPositiveMembers (undefined :: u))

         go _ _ []         = error "toUnum: empty member list"
         go i b [x1]       = if b == x1 then (i,True) else (i,False)
         go i b (x1:x2:xs) = if
            | b < x1    -> error "toUnum: invalid number"
            | b == x1   -> (i,True)
            | b < x2    -> (i,False)
            | otherwise -> go (i+1) b (x2:xs)

-- | Return a Rational and the uncertainty bit from a number
--
-- TODO: currently we use Set.elemAt which is O(log n). We may want to use an
-- O(1) array generated statically if possible.
fromUnum :: forall u.
   ( Unum u
   , FiniteBits (UnumWord u)
   , Integral (UnumWord u)
   , KnownNat (UnumBitCount u)
   ) => u -> (Rational,Bool)
fromUnum u = (r, unumIsExactNumber u)
   where
      w = unumUnpack u `shiftR` 1
      signed = testBit w (fromIntegral (unumBitCount @u - 2))
      i      = fromIntegral <| clearBit w (fromIntegral (unumBitCount @u - 2))
      r = if
         | signed && i == 0 -> infinity
         |           i == 0 -> 0
         | signed           -> Set.elemAt (i-1) (unumNegativeMembers (undefined :: u))
         | otherwise        -> Set.elemAt (i-1) (unumPositiveMembers (undefined :: u))
   
-------------------------------------------------
-- SORNs

-- | A set of numbers
data UnumSet u
   = AnySet (SORN u)
--   | ConnectedSet (CSORN u) -- TODO: connected sets (compressed)


instance Show (SORN u) => Show (UnumSet u) where
   show (AnySet su) = show su

-- | Set of Real Numbers (SORN)
newtype SORN u = SORN (SORNWord u)

instance Eq (SORNWord u) => Eq (SORN u) where
   (==) (SORN a) (SORN b) = a == b

instance Ord (SORNWord u) => Ord (SORN u) where
   compare (SORN a) (SORN b) = compare a b

instance
   ( Show u
   , Unum u
   , KnownNat (UnumBitCount u)
   , Num (UnumWord u)
   , FiniteBits (UnumWord u)
   , Eq (SORNWord u)
   , FiniteBits (SORNWord u)
   ) => Show (SORN u)
   where
      show su = "fromList " ++ show (sornElems su)

type family SORNSize u where
   SORNSize u = Pow 2 (UnumBitCount u)

type family SORNWord u where
   SORNWord u = WordAtLeast (SORNSize u)

-- | Mask a SORN Word
sornMask :: forall u.
   ( Unum u
   , FiniteBits (SORNWord u)
   , KnownNat (SORNSize u)
   ) => SORNWord u -> SORNWord u
sornMask = maskLeastBits (natValue @(SORNSize u))

-- | Return SORN elements
sornElems :: forall u.
   ( Unum u
   , KnownNat (UnumBitCount u)
   , Num (UnumWord u)
   , Eq (SORNWord u)
   , FiniteBits (SORNWord u)
   ) => SORN u -> [u]
sornElems (SORN su) = go su
   where
      go w
         | w == zeroBits = []
         | otherwise     = unumPack w' : go (clearBit w c)
            where
               w' = fromIntegral c :: UnumWord u
               c = countTrailingZeros w

-- | Empty SORN
sornEmpty ::
   ( Unum u
   , FiniteBits (SORNWord u)
   ) => SORN u
sornEmpty = SORN zeroBits

-- | Full SORN
sornFull :: forall u.
   ( Unum u
   , FiniteBits (SORNWord u)
   , KnownNat (SORNSize u)
   ) => SORN u
sornFull = SORN (sornMask @u (complement zeroBits))

-- | Insert element into a SORN
sornInsert ::
   ( Unum u
   , FiniteBits (SORNWord u)
   , Integral (UnumWord u)
   ) => SORN u -> u -> SORN u
sornInsert (SORN w) u = SORN (setBit w (fromIntegral (unumUnpack u)))

-- | Remove element into a SORN
sornRemove ::
   ( Unum u
   , FiniteBits (SORNWord u)
   , Integral (UnumWord u)
   ) => SORN u -> u -> SORN u
sornRemove (SORN w) u = SORN (clearBit w (fromIntegral (unumUnpack u)))

-- | Create a SORN from a list
sornFromList ::
   ( Unum u
   , Integral (UnumWord u)
   , FiniteBits (SORNWord u)
   ) => [u] -> SORN u
sornFromList = List.foldl' sornInsert sornEmpty 

-- | Union of the SORNs
sornUnions :: forall u.
   ( FiniteBits (SORNWord u)
   , Unum u
   ) => [SORN u] -> SORN u
sornUnions = List.foldl' sornUnion sornEmpty 

-- | Union of two SORNs
sornUnion ::
   ( Unum u
   , FiniteBits (SORNWord u)
   ) => SORN u -> SORN u -> SORN u
sornUnion (SORN x) (SORN y) = SORN (x .|. y)


-------------------------------------------------
-- Operation tables

unumRange ::
   ( Unum u
   , Num (UnumWord u)
   , FiniteBits (UnumWord u)
   , KnownNat (UnumBitCount u)
   ) => u -> u -> [u]
unumRange x1 x2
   | x1 == x2  = [x1]
   | otherwise = x1 : unumRange (unumNext x1) x2

-- | Add exact unums.
-- Warning: infinity + infinity = infinity
unumAddExact :: forall u.
   ( Unum u
   , Integral (UnumWord u)
   , FiniteBits (UnumWord u)
   , KnownNat (UnumBitCount u)
   ) => u -> u -> u
unumAddExact u1 u2 = if
   -- inf + x -> inf
   | r1 == infinity && c1  -> u1
   | r2 == infinity && c2  -> u2
   -- x + y -> z
   | c1 && c2              -> toUnum (r1+r2)
   | otherwise             -> error "unumAddExact: invalid numbers"
   where
      (r1,c1) = fromUnum u1
      (r2,c2) = fromUnum u2

-- | Unum addition
-- TODO: use connected sets (compressed)
unumAdd :: forall u.
   ( Unum u
   , Integral (UnumWord u)
   , FiniteBits (UnumWord u)
   , KnownNat (UnumBitCount u)
   , FiniteBits (SORNWord u)
   , KnownNat (SORNSize u)
   ) => u -> u -> UnumSet u
unumAdd u1 u2 = AnySet $ if
   -- inf + inf -> full set
   | r1 == infinity && c1 &&
     r2 == infinity && c2 -> sornFull
   | c1 && c2             -> sornFromList [unumAddExact u1 u2]
   -- ~x +  y -> [~u..~v]
   --  x + ~y -> [~u..~v]
   -- ~x + ~y -> [~u..~v]
   | otherwise            -> sornFromList (unumRange lowb highb)
   where
      (r1,c1) = fromUnum u1
      (r2,c2) = fromUnum u2

      lowb  = unumNext (unumAddExact (unumLowerBound u1) (unumLowerBound u2))
      highb = unumPrev (unumAddExact (unumUpperBound u1) (unumUpperBound u2))


-- | Unum multiplication
-- TODO: use connected sets (compressed)
unumMul :: forall u.
   ( Unum u
   , Show u
   , Integral (UnumWord u)
   , FiniteBits (UnumWord u)
   , KnownNat (UnumBitCount u)
   , FiniteBits (SORNWord u)
   , KnownNat (SORNSize u)
   ) => u -> u -> UnumSet u
unumMul u1 u2 = AnySet $ if
   -- inf * inf -> inf
   | r1 == infinity && c1 &&
     r2 == infinity && c2 -> sornFromList [u1]
   -- inf * 0 -> empty set
   | r1 == infinity && c1 &&
     r2 == 0        && c2 -> sornEmpty
   | r2 == infinity && c2 &&
     r1 == 0        && c1 -> sornEmpty
   -- inf * x -> inf
   | r1 == infinity && c1 -> sornFromList [u1]
   | r2 == infinity && c2 -> sornFromList [u2]
   -- x * y -> z
   | c1 && c2             -> sornFromList [toUnum (r1*r2)]
   -- ~x *  y -> [~u..~v]
   --  x * ~y -> [~u..~v]
   -- ~x * ~y -> [~u..~v]
   | otherwise            -> case resSign of
      Positive -> sornFromList (unumRange lowb highb)
      Negative -> sornFromList (unumRange (unumNegate highb) (unumNegate lowb))
      NoSign   -> error ("unumMul: invalid numbers: " ++ show (lowb, highb))

   where
      (resSign,u1',u2') = case (unumSign u1, unumSign u2) of
         (Negative,Negative) -> (Positive, unumNegate u1, unumNegate u2)
         (Negative,Positive) -> (Negative, unumNegate u1, u2)
         (Positive,Negative) -> (Negative, u1, unumNegate u2)
         _                   -> (NoSign, u1, u2)
      (r1,c1) = fromUnum u1
      (r2,c2) = fromUnum u2
      minB = fst . fromUnum . unumLowerBound
      maxB = fst . fromUnum . unumUpperBound
      lowb  = unumNext (toUnum (minB u1' * minB u2'))
      highb = unumPrev $ if maxB u1' == infinity
         then unumInfinity
         else if maxB u2' == infinity
            then unumInfinity
            else toUnum (maxB u1' * maxB u2')


-- | Table for addition
-- TODO: use connected sets (compressed)
--newtype TableAdd u = TableAdd (V.Vector (UnumSet u))

-------------------------------------------------
-- Default numeric systems

-- | 2-bit Unum
newtype Unum2b = Unum2b Word8 deriving (Show,Eq,Ord)

instance Unum Unum2b where
   type UnumBitCount Unum2b = 2
   unumPack                 = Unum2b
   unumUnpack (Unum2b x)    = x
   unumInputMembers _       = Set.fromList []

-- | 3-bit Unum
newtype Unum3b = Unum3b Word8 deriving (Show,Eq,Ord)

instance Unum Unum3b where
   type UnumBitCount Unum3b = 3
   unumPack                 = Unum3b
   unumUnpack (Unum3b x)    = x
   unumInputMembers _       = Set.fromList [1]
