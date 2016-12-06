{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}

-- | Storable class
module ViperVM.Format.Binary.Storable
   ( Storable (..)
   , IsStorable
   , makeStorable
   , Padding
   , PaddingVal
   , PaddingEx
   , Plus
   , Max
   , natVal
   -- * Storable
   , peek
   , poke
   , sizeOf'
   , sizeOfT
   , sizeOfT'
   , alignment'
   , alignmentT
   , alignmentT'
   , peekByteOff
   , pokeByteOff
   , peekElemOff
   , pokeElemOff
   , alloca
   , allocaBytes
   , allocaBytesAligned
   , malloc
   , with
   , withMany
   , allocaArray
   , mallocArray
   , withArray
   , withArrayLen
   , peekArray
   , pokeArray
   )
where

#include "MachDeps.h"

import qualified Foreign.Storable as FS
import Foreign.C.Types (CSize,CChar,CULong,CLong,CUInt,CInt,CUShort,CShort)
import qualified Foreign.Marshal.Alloc as P

import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Ptr
import ViperVM.Utils.Types
import ViperVM.Utils.Types.List (Max)
import ViperVM.Utils.Flow
import ViperVM.Utils.List
import ViperVM.Utils.Maybe
import Language.Haskell.TH.Syntax
import Language.Haskell.TH

type IsStorable a =
   ( Storable a
   , KnownNat (SizeOf a)
   , KnownNat (Alignment a)
   )

-- | A storable data in constant space whose size is known at compile time
class Storable a where
   -- | Size of the stored data (in bytes)
   type SizeOf a    :: Nat

   -- | Alignment requirement (in bytes)
   type Alignment a :: Nat

   -- | Peek (read) a value from a memory address
   peekIO :: Ptr a -> IO a

   -- | Poke (write) a value at the given memory address
   pokeIO :: Ptr a -> a -> IO ()


-- | Compute the required padding between the size sz and b to respect b's alignment
type family Padding (sz :: Nat) b where
   Padding sz b = PaddingVal sz (Alignment b)

-- | Compute the required padding between the size sz and the alignment b
type family PaddingVal (sz :: Nat) (b :: Nat) where
   PaddingVal sz b = PaddingEx (Modulo sz b) b

type family PaddingEx (m :: Nat) (a :: Nat) where
   PaddingEx 0 a = 0
   PaddingEx m a = a - m

-- TODO: remove this alias when TH is able to find (+) at the type level
type family Plus a b where
   Plus a b = (a + b)


-- | Get statically known size
sizeOf :: forall a. (IsStorable a) => a -> Word
sizeOf _ = natValue' @(SizeOf a)

-- | Get statically known alignment
alignment :: forall a. (IsStorable a) => a -> Word
alignment _ = natValue' @(Alignment a)

-- | Generalized 'sizeOf'
sizeOf' :: (Integral b, IsStorable a) => a -> b
{-# INLINE sizeOf' #-}
sizeOf' = fromIntegral . sizeOf

-- | SizeOf (for type-application)
sizeOfT :: forall a. (IsStorable a) => Word
{-# INLINE sizeOfT #-}
sizeOfT = sizeOfT' @a

-- | SizeOf' (for type-application)
sizeOfT' :: forall a b. (IsStorable a, Integral b) => b
{-# INLINE sizeOfT' #-}
sizeOfT' = natValue @(SizeOf a)

-- | Generalized 'alignment'
alignment' :: (Integral b, IsStorable a) => a -> b
{-# INLINE alignment' #-}
alignment' = fromIntegral . alignment

-- | Alignment (for type-application)
alignmentT :: forall a. (Storable a, KnownNat (Alignment a)) => Word
{-# INLINE alignmentT #-}
alignmentT = alignmentT' @a

-- | Alignment' (for type-application)
alignmentT' :: forall a b. (Storable a, Integral b, KnownNat (Alignment a)) => b
{-# INLINE alignmentT' #-}
alignmentT' = natValue @(Alignment a)

-- | Peek a value from a pointer
peek :: (Storable a, MonadIO m) => Ptr a -> m a
peek p = liftIO (peekIO p)

-- | Poke a value to a pointer
poke :: (Storable a, MonadIO m) => Ptr a -> a -> m ()
poke p v = liftIO (pokeIO p v)

-- | Peek with byte offset
peekByteOff :: (MonadIO m, Storable a) => Ptr a -> Int -> m a
{-# INLINE peekByteOff #-}
peekByteOff ptr off = peek (ptr `indexPtr` off)

-- | Poke with byte offset
pokeByteOff :: (MonadIO m, Storable a) => Ptr a -> Int -> a -> m ()
{-# INLINE pokeByteOff #-}
pokeByteOff ptr off = poke (ptr `indexPtr` off)

-- | Peek with element size offset
peekElemOff :: forall a m. (MonadIO m, IsStorable a) => Ptr a -> Int -> m a
peekElemOff ptr off = peekByteOff ptr (off * sizeOfT' @a)

-- | Poke with element size offset
pokeElemOff :: (MonadIO m, IsStorable a) => Ptr a -> Int -> a -> m ()
pokeElemOff ptr off val = pokeByteOff ptr (off * sizeOf' val) val

-- | Allocate some bytes
allocaBytes :: MonadInIO m => Word -> (Ptr a -> m b) -> m b
allocaBytes sz = liftWith (P.allocaBytes (fromIntegral sz))

-- | Allocate some aligned bytes
allocaBytesAligned :: MonadInIO m => Word -> Word -> (Ptr a -> m b) -> m b
allocaBytesAligned sz align = liftWith (P.allocaBytesAligned (fromIntegral sz) (fromIntegral align))

-- | @'alloca' f@ executes the computation @f@, passing as argument
-- a pointer to a temporarily allocated block of memory sufficient to
-- hold values of type @a@.
--
-- The memory is freed when @f@ terminates (either normally or via an
-- exception), so the pointer passed to @f@ must /not/ be used after this.
--
alloca :: forall a b m. (MonadInIO m, IsStorable a) => (Ptr a -> m b) -> m b
{-# INLINE alloca #-}
alloca = allocaBytesAligned (sizeOfT' @a) (alignmentT' @a)

-- | Allocate a block of memory that is sufficient to hold values of type
-- @a@. The size of the area allocated is determined by the 'sizeOf'
-- method from the instance of 'Storable' for the appropriate type.
--
-- The memory may be deallocated using 'free' or 'finalizerFree' when
-- no longer required.
malloc :: forall a m. (MonadIO m, IsStorable a) => m (Ptr a)
{-# INLINE malloc #-}
malloc = liftIO (mallocBytes (sizeOfT @a))

-- | @'with' val f@ executes the computation @f@, passing as argument
-- a pointer to a temporarily allocated block of memory into which
-- @val@ has been marshalled (the combination of 'alloca' and 'poke').
--
-- The memory is freed when @f@ terminates (either normally or via an
-- exception), so the pointer passed to @f@ must /not/ be used after this.
with :: (MonadInIO m, IsStorable a) => a -> (Ptr a -> m b) -> m b
{-# INLINE with #-}
with val f =
   alloca $ \ptr -> do
      poke ptr val
      f ptr

-- | Temporarily allocate space for the given number of elements
-- (like 'alloca', but for multiple elements).
allocaArray :: forall a b m. (MonadInIO m, IsStorable a) => Word -> (Ptr a -> m b) -> m b
allocaArray size = liftWith (allocaBytesAligned (size * sizeOfT' @a) (alignmentT' @a))

-- | Allocate space for the given number of elements
-- (like 'malloc', but for multiple elements).
mallocArray :: forall a m. (MonadIO m, IsStorable a) => Word -> m (Ptr a)
mallocArray size = mallocBytes (size * sizeOfT @a)

-- | Convert an array of given length into a Haskell list.  The implementation
-- is tail-recursive and so uses constant stack space.
peekArray :: (MonadIO m, IsStorable a) => Word -> Ptr a -> m [a]
peekArray size ptr
   | size <= 0 = return []
   | otherwise = f (size-1) []
  where
    f 0 acc = (:acc) <$> peekElemOff ptr 0
    f n acc = f (n-1) =<< ((:acc) <$> peekElemOff ptr (fromIntegral n))

-- | Write the list elements consecutive into memory
pokeArray :: (MonadIO m, IsStorable a) => Ptr a -> [a] -> m ()
pokeArray ptr vals0 = go vals0 0
  where go [] _         = return ()
        go (val:vals) n = do pokeElemOff ptr n val; go vals (n+1)

-- | Temporarily store a list of storable values in memory
-- (like 'with', but for multiple elements).
withArray :: (MonadInIO m, IsStorable a) => [a] -> (Ptr a -> m b) -> m b
withArray vals = withArrayLen vals . const

-- | Like 'withArray', but the action gets the number of values
-- as an additional parameter
withArrayLen :: (MonadInIO m, IsStorable a) => [a] -> (Word -> Ptr a -> m b) -> m b
withArrayLen vals f  =
  allocaArray len $ \ptr -> do
      pokeArray ptr vals
      f len ptr
  where
    len = fromIntegral (length vals)

-- | Replicates a @withXXX@ combinator over a list of objects, yielding a list of
-- marshalled objects
withMany :: (a -> (b -> res) -> res)  -- withXXX combinator for one object
         -> [a]                       -- storable objects
         -> ([b] -> res)              -- action on list of marshalled obj.s
         -> res
withMany _       []     f = f []
withMany withFoo (x:xs) f = withFoo x $ \x' ->
                              withMany withFoo xs (\xs' -> f (x':xs'))

-- | Generalize FS.peek
fsPeek :: (FS.Storable a, MonadIO m) => Ptr a -> m a
fsPeek = liftIO . FS.peek

-- | Generalize FS.poke
fsPoke :: (FS.Storable a, MonadIO m) => Ptr a -> a -> m ()
fsPoke ptr a = liftIO (FS.poke ptr a)

instance Storable Word where
   type SizeOf    Word = SIZEOF_HSWORD
   type Alignment Word = ALIGNMENT_HSWORD
   peekIO              = fsPeek
   pokeIO              = fsPoke

instance Storable Word8 where
   type SizeOf    Word8 = 1
   type Alignment Word8 = 1
   peekIO               = fsPeek
   pokeIO               = fsPoke

instance Storable Word16 where
   type SizeOf    Word16 = 2
   type Alignment Word16 = 2
   peekIO                = fsPeek
   pokeIO                = fsPoke

instance Storable Word32 where
   type SizeOf    Word32 = 4
   type Alignment Word32 = 4
   peekIO                = fsPeek
   pokeIO                = fsPoke

instance Storable Word64 where
   type SizeOf    Word64 = 8
   type Alignment Word64 = 8
   peekIO                = fsPeek
   pokeIO                = fsPoke

instance Storable Int where
   type SizeOf    Int = SIZEOF_HSINT
   type Alignment Int = ALIGNMENT_HSINT
   peekIO             = fsPeek
   pokeIO             = fsPoke

instance Storable Int8 where
   type SizeOf    Int8 = 1
   type Alignment Int8 = 1
   peekIO              = fsPeek
   pokeIO              = fsPoke

instance Storable Int16 where
   type SizeOf    Int16 = 2
   type Alignment Int16 = 2
   peekIO               = fsPeek
   pokeIO               = fsPoke

instance Storable Int32 where
   type SizeOf    Int32 = 4
   type Alignment Int32 = 4
   peekIO               = fsPeek
   pokeIO               = fsPoke

instance Storable Int64 where
   type SizeOf    Int64 = 8
   type Alignment Int64 = 8
   peekIO               = fsPeek
   pokeIO               = fsPoke

instance Storable (Ptr a) where
   type SizeOf    (Ptr a) = SIZEOF_HSPTR
   type Alignment (Ptr a) = ALIGNMENT_HSPTR
   peekIO                 = fsPeek
   pokeIO                 = fsPoke

instance Storable Float where
   type SizeOf    Float = 4
   type Alignment Float = 4
   peekIO               = fsPeek
   pokeIO               = fsPoke

instance Storable Double where
   type SizeOf    Double = 8
   type Alignment Double = 8
   peekIO                = fsPeek
   pokeIO                = fsPoke


-- | Create a storable instance from a data type declaration
--
-- Use it with: $(makeStorable ''MyStruct)
--
-- Note that we don't support parametric fields for now
makeStorable :: Name -> Q [Dec]
makeStorable dtypName = do
   exts <- extsEnabled

   when (DataKinds `notElem` exts) $
      fail "Please enable DataKinds extension to use makeStorable"
   when (TypeFamilies `notElem` exts) $
      fail "Please enable TypeFamilies extension to use makeStorable"

   let errmsg = "Cannot derive a Storable instance for " ++ show dtypName ++ "."
           ++ "\n Maybe because it is not a valid data type (more than one constructor, has type parameters, etc.)"
           ++ "\n Did you remember to double-tick the type as in"
           ++ "\n $(makeStorable ''MyStruct)?"

   let
      extractTypeVarNames (PlainTV n)    = n
      extractTypeVarNames (KindedTV n _) = n

      tryFullInstance name fieldsTypes = do
         -- Try to get SizeOf and Alignment of fields as Integer
         alSzs <- forM fieldsTypes $ \t -> do
                  szInst <- reifyInstances (mkName "SizeOf") [t]
                  alInst <- reifyInstances (mkName "Alignment") [t]
                  
                  let extractVal [TySynInstD _ (TySynEqn _ (LitT (NumTyLit x)))] = Just x
                      extractVal _ = Nothing
                  case (extractVal alInst, extractVal szInst) of
                     (Just x, Just y) -> return (Just (x,y))
                     _                -> return Nothing

         if any isNothing alSzs
            then makeGenericInstance dtypName name fieldsTypes
            else makeFullInstance dtypName name (fmap fromJust alSzs)

   -- extract constructor name and fields
   reify dtypName >>= \case
      TyConI (DataD [] _ [] _ [RecC name fs] _) ->
         tryFullInstance name (fmap (\(_,_,t) -> t) fs)
      TyConI (DataD [] _ [] _ [NormalC name fs] _) ->
         tryFullInstance name (fmap snd fs)
      TyConI (NewtypeD [] _ tvbs _ (NormalC name [fs]) _) -> do
         makeNewtypeInstance dtypName name (snd fs) 
            (fmap extractTypeVarNames tvbs)
      _                                         -> fail errmsg 

-- | Create an instance with all the offsets/sizes/alignments as values
makeNewtypeInstance :: Name -> Name -> Type -> [Name] -> Q [Dec]
makeNewtypeInstance dtypName cname fieldType tvs = do
   exts <- extsEnabled

   when (FlexibleContexts `notElem` exts) $
      fail "Please enable FlexibleContexts extension to use makeStorable"
   when (UndecidableInstances `notElem` exts) $
      fail "Please enable UndecidableInstances extension to use makeStorable"

   cls  <- conT (mkName "Storable")
   dtyp <- conT dtypName

   -- patterns for the pointer and for the fields
   ptrN <- newName "ptr"
   varN <- newName "a"
   let ptrP  = VarP ptrN
       varsP = ConP cname [VarP varN]

   -- generate peek/poke methods
   let 
       dtypE = conE cname
       varaE = varE varN
       ptrE  = varE ptrN
       context    = [AppT (ConT (mkName "IsStorable")) fieldType]

   peek1 <- [| $(dtypE) <$> peekIO (castPtr $(ptrE)) |]
   poke1 <- [| pokeIO (castPtr $(ptrE)) $(varaE)     |]

   -- build parameterized data type
   let dtyp' = foldl' AppT dtyp (fmap VarT tvs)

   -- generate the instance
   let inst = InstanceD Nothing context
               (AppT cls dtyp')
                  [ FunD (mkName "peekIO")
                     [ Clause [ptrP] (NormalB peek1) [] ]
                  , FunD (mkName "pokeIO")
                     [ Clause [ptrP, varsP] (NormalB poke1) [] ]
                  , TySynInstD (mkName "Alignment") $ TySynEqn [dtyp']
                     (AppT (ConT (mkName "Alignment")) fieldType) 
                  , TySynInstD (mkName "SizeOf") $ TySynEqn [dtyp']
                     (AppT (ConT (mkName "SizeOf")) fieldType)
                  ]

   return [inst]
   

-- | Create an instance with all the offsets/sizes/alignments as values
makeFullInstance :: Name -> Name -> [(Integer,Integer)] -> Q [Dec]
makeFullInstance dtypName cname alSzs = do
   cls  <- conT (mkName "Storable")
   dtyp <- conT dtypName
   app1 <- [| (<$>) |]
   app2 <- [| (<*>) |]
   ret  <- [| return () |]
   ret1 <- [| return |]

   -- patterns for the pointer and for the fields
   ptrN  <- newName "ptr"
   varsN <- forM [1..length alSzs] $ \i -> newName ("a"++show i)
   let ptrP = case alSzs of
                  [] -> WildP    -- avoid "`ptr` declared but not used" errors
                  _  -> VarP ptrN
       ptrE  = varE ptrN
       varsP = ConP cname (fmap VarP varsN)
       varsE = fmap VarE varsN

   -- compute field offsets/alignments and structure size/alignment
   let
      -- | Compute field offsets from a list of (Alignment,Size)
      --
      -- The last offset in the list is the structure size correctly padded for its
      -- own alignment.
      computeOffsets :: Integer -> [(Integer,Integer)] -> [Integer]
      computeOffsets structAlign xs = go 0 (xs ++ [(structAlign,0)])
         where
            go off ys@((_,sz):(align,_):_) = let al = ((align - off) `mod` align) in
                                          (off + al) : go (off + al + sz) (tail ys)
            go off [(align,_)]             = let al = ((align - off) `mod` align) in [off + al]
            go _   _                       = []

      alignmnt   = maximum (1:fmap fst alSzs)
      allOffsets = computeOffsets alignmnt alSzs
      offsets    = init allOffsets -- trim structure whole size
      sizeof     = last allOffsets
      context    = []

   -- generate peek/poke methods
   let 
       peek1 :: Integer -> Q Exp
       peek1 x = [| peekByteOff (castPtr $(ptrE)) x |]

       poke1 :: Exp -> Integer -> Q Exp
       poke1 a x = [| pokeByteOff (castPtr $(ptrE)) x $(return a) |]

       doPeek []     = AppE ret1 (ConE cname)
       doPeek (x:xs) = foldl f (UInfixE (ConE cname) app1 x) xs
         where
            f y v = UInfixE y app2 v

       doPoke [] = ret
       doPoke xs = DoE (fmap NoBindS xs)

   poks  <- doPoke <$> traverse (\(off,v) -> poke1 v off) (offsets `zip` varsE)
   peeks <- doPeek <$> traverse peek1 offsets

   -- generate the instance
   let inst = InstanceD Nothing context
               (AppT cls dtyp)
                  [ FunD (mkName "peekIO")
                     [ Clause [ptrP] (NormalB peeks) [] ]
                  , FunD (mkName "pokeIO")
                     [ Clause [ptrP, varsP] (NormalB poks) [] ]
                  , TySynInstD (mkName "Alignment") $ TySynEqn [ConT dtypName]
                     (LitT (NumTyLit alignmnt))
                  , TySynInstD (mkName "SizeOf") $ TySynEqn [ConT dtypName]
                     (LitT (NumTyLit sizeof))
                  ]

   return [inst]


-- | Create an instance with all the offsets/sizes/alignments as type-level
-- values
makeGenericInstance :: Name -> Name -> [Type] -> Q [Dec]
makeGenericInstance dtypName cname fieldsTypes = do
   exts <- extsEnabled

   when (FlexibleContexts `notElem` exts) $
      fail "Please enable FlexibleContexts extension to use makeStorable"
   when (UndecidableInstances `notElem` exts) $
      fail "Please enable UndecidableInstances extension to use makeStorable"

   cls  <- conT (mkName "Storable")
   dtyp <- conT dtypName
   app1 <- [| (<$>) |]
   app2 <- [| (<*>) |]
   ret  <- [| return () |]
   ret1 <- [| return |]
   -- TODO: directly use `natValue' instead of `Proxy' when TypeApplications
   -- becomes supported by TH (2.12)
   sizeofT <- conT (mkName "SizeOf")
   alignT  <- conT (mkName "Alignment")

   let
      numTyLitT = LitT . NumTyLit
      natValE t = [| fromIntegral (natVal (Proxy :: Proxy $(return t))) |]

   -- patterns for the pointer and for the fields
   ptrN  <- newName "ptr"
   varsN <- forM [1..length fieldsTypes] $ \i -> newName ("a"++show i)
   let ptrP = case fieldsTypes of
                  [] -> WildP    -- avoid "`ptr` declared but not used" errors
                  _  -> VarP ptrN
       ptrE  = varE ptrN
       varsP = ConP cname (fmap VarP varsN)
       varsE = fmap VarE varsN

   -- compute field offsets/alignments and structure size/alignment
   let
      szs = fmap (AppT sizeofT) fieldsTypes
      als = fmap (AppT alignT)  fieldsTypes
      mkPad off align = ConT (mkName "PaddingVal") `AppT` off `AppT` align
      plus a b = ConT (mkName "Plus") `AppT` a `AppT` b
      toListT []     = PromotedNilT
      toListT (x:xs) = PromotedConsT `AppT` x `AppT` toListT xs

      computeOffsets :: Type -> [(Type,Type)] -> [Type]
      computeOffsets structAlign xs = go (numTyLitT 0) (xs ++ [(structAlign,numTyLitT 0)])
         where
            go off ys@((_,sz):(align,_):_) = let al = mkPad off align in
                                          (off `plus` al) : go (off `plus` al `plus` sz) (tail ys)
            go off [(align,_)]             = [off `plus` mkPad off align]
            go _   _                       = []

      alignmnt   = ConT (mkName "Max") `AppT` toListT (numTyLitT 1:als)
      allOffsets = computeOffsets alignmnt (als `zip` szs)
      offsets    = init allOffsets -- trim structure whole size
      sizeof     = last allOffsets
      context    = []

   -- generate peek/poke methods
   let 
       peek1 :: Type -> Q Exp
       peek1 x = [| peekByteOff (castPtr $(ptrE)) $(natValE x) |]

       poke1 :: Exp -> Type -> Q Exp
       poke1 a x = [| pokeByteOff (castPtr $(ptrE)) $(natValE x) $(return a) |]

       doPeek []     = AppE ret1 (ConE cname)
       doPeek (x:xs) = foldl f (UInfixE (ConE cname) app1 x) xs
         where
            f y v = UInfixE y app2 v

       doPoke [] = ret
       doPoke xs = DoE (fmap NoBindS xs)

   poks  <- doPoke <$> traverse (\(off,v) -> poke1 v off) (offsets `zip` varsE)
   peeks <- doPeek <$> traverse peek1 offsets

   -- generate the instance
   let inst = InstanceD Nothing context
               (AppT cls dtyp)
                  [ FunD (mkName "peekIO")
                     [ Clause [ptrP] (NormalB peeks) [] ]
                  , FunD (mkName "pokeIO")
                     [ Clause [ptrP, varsP] (NormalB poks) [] ]
                  , TySynInstD (mkName "Alignment") $ TySynEqn [ConT dtypName] alignmnt
                  , TySynInstD (mkName "SizeOf") $ TySynEqn [ConT dtypName] sizeof
                  ]

   return [inst]
