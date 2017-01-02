{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Haskus.Format.Binary.Unum2
   ( IsUnum (..)
   )
where

import Data.Set as Set
import GHC.Real
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Bits

class IsUnum x where
   -- | Number of bits to store a number
   type UnumBitCount x :: Nat

   type UnumWord x = WordAtLeast (UnumBitCount x)

   -- Strictly positive exact members of the numeric system without their reciprocals
   --
   -- We then include the inverse and the reciprocal of every number (0
   -- and its reciprocal are included too)
   unumInputMembers :: x -> Set Rational

   -- | All the exact members of the numeric system (including `infinity`)
   unumMembers :: x -> Set Rational
   unumMembers x = Set.unions
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
   unumNegativeMembers x = Set.map (0 -) poss

 
-- | Reciprocate
rcp :: Rational -> Rational
rcp (n :% d) = d :% n


data Unum a = U (UnumWord a)

unumIsOpenInterval :: FiniteBits (UnumWord a) => Unum a -> Bool
unumIsOpenInterval (U w) = testBit w 0

unumIsExactNumber :: FiniteBits (UnumWord a) => Unum a -> Bool
unumIsExactNumber = not . unumIsOpenInterval

unumBitCount :: forall u. (KnownNat (UnumBitCount u)) => Word
unumBitCount = natValue @(UnumBitCount u)

-- | Negate a number
unumNegate :: FiniteBits (UnumWord a) => Unum a -> Unum a
{-# INLINE unumNegate #-}
unumNegate (U w) = U (maskLeastBits s (complement w + 1))
   where
      s = unumBitCount @u

-- | Reciprocate a number
unumReciprocate :: forall u.
   ( FiniteBits (UnumWord u)
   , Num (UnumWord u)
   , KnownNat (UnumBitCount u)
   ) => U u -> U u
{-# INLINE unumReciprocate #-}
unumReciprocate (U w) = U (w `xor` m + 1)
   where
      s = unumBitCount @u
      m = makeMask (s-1)

