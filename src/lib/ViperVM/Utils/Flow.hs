{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Control flow
module ViperVM.Utils.Flow
   ( flowSeq
   , flowSeqE
   , flowSeqM
   , flowSeq'
   , flowMSeq
   , flowMSeqM
   , flowMTraceM
   , flowFinally
   , flowFinallyV
   , flowFinallyM
   , flowMatch
   , flowRetry
   , flowBind
   , flowCatch
   )
where

import ViperVM.Utils.Variant
import ViperVM.Utils.HList

import Control.Monad
import Data.HList.HList
import Data.Proxy
import GHC.TypeLits
import Data.Foldable (traverse_)

-- | Bind two variants in sequence. By convention, we choose the first variant
-- value/type as the valid one.
-- (i.e. generalized EitherT binding with reversed parameters)
flowSeq :: forall x xs m l l2 k .
   ( l2 ~ ReplaceAt 0 (x ': xs) l
   , Monad m
   , k ~ Length l
   , KnownNat k
   )
   => m (Variant (x ': xs)) -> (x -> m (Variant l)) -> m (Variant l2)
flowSeq v f = updateVariantFoldM (Proxy :: Proxy 0) f =<< v

-- | Lift an Either into a flow
flowSeqE :: forall x (xs :: [*]) m a b.
   ( Monad m
   ) => m (Variant (x ': xs)) -> (x -> m (Either a b)) -> m (Variant (b ': a ': xs))
flowSeqE v f = flowSeq v (liftEitherM . f)

-- | Lift a non-failing function into a flow
flowSeqM :: forall x (xs :: [*]) m a.
   ( Monad m
   ) => m (Variant (x ': xs)) -> (x -> m a) -> m (Variant (a ': xs))
flowSeqM v f = v `flowSeq` f'
   where
      f' :: x -> m (Variant '[a])
      f' x = setVariant0 <$> f x


-- | Like `flowSeq` but specialised for `()` (similarly to `>>`)
flowSeq' :: forall xs m l l2 k .
   ( l2 ~ ReplaceAt 0 (() ': xs) l
   , Monad m
   , k ~ Length l
   , KnownNat k
   )
   => m (Variant (() ': xs)) -> m (Variant l) -> m (Variant l2)
flowSeq' v f = flowSeq v (const f)

-- | Compose like (>=>)
flowMSeq :: forall x xs m l l2 k a.
   ( l2 ~ ReplaceAt 0 (x ': xs) l
   , Monad m
   , k ~ Length l
   , KnownNat k
   )
   => (a -> m (Variant (x ': xs))) -> (x -> m (Variant l)) -> (a -> m (Variant l2))
flowMSeq v f = updateVariantFoldM (Proxy :: Proxy 0) f <=< v

-- | Lift a non-failing function into a flow
flowMSeqM :: forall x (xs :: [*]) m a b.
   ( Monad m
   ) => (b -> m (Variant (x ': xs))) -> (x -> m a) -> (b -> m (Variant (a ': xs)))
flowMSeqM v f = v `flowMSeq` f'
   where
      f' :: x -> m (Variant '[a])
      f' x = setVariant0 <$> f x

-- | Lift a non-failing tracing function into a flow
flowMTraceM :: forall x (xs :: [*]) m b.
   ( Monad m
   ) => (b -> m (Variant (x ': xs))) -> (x -> m ()) -> (b -> m (Variant (x ': xs)))
flowMTraceM v f b = do
   r <- v b
   traverse_ f (getVariant0 r)
   return r

-- | Execute a statement regardless of the failure of the previous one
--
-- As we cannot report errors both in `xs` and `ys`, errors in `ys` are only
-- reported if `v` succeeds (i.e. there is no error in `xs`).
--
-- Said differently: errors in the inner block have the priority on errors in
-- the finally block
flowFinally :: forall m x xs ys.
   ( Monad m
   , KnownNat (Length ys)
   ) => m (Variant (x ': xs)) -> (Maybe x -> m (Variant ys)) -> m (Variant (Concat ys xs))
flowFinally v f = do
   r  <- v
   updateVariantFoldM (Proxy :: Proxy 0) (const (f (getVariant0 r))) r

-- | Similar to `flowFinally`, but pass the whole variant to the finally block
flowFinallyV :: forall m x xs ys.
   ( Monad m
   , KnownNat (Length ys)
   ) => m (Variant (x ': xs)) -> (Variant (x ': xs) -> m (Variant ys)) -> m (Variant (Concat ys xs))
flowFinallyV v f = do
   r  <- v
   updateVariantFoldM (Proxy :: Proxy 0) (const (f r)) r

-- | Execute a finally block that does not fail
flowFinallyM :: forall m x xs.
   ( Monad m
   ) => m (Variant (x ': xs)) -> (Maybe x -> m ()) -> m (Variant (x ': xs))
flowFinallyM v f = do
   r  <- v
   f (getVariant0 r)
   return r

-- | Match a variant by using a tuple
flowMatch :: forall l t m a l2 is.
   ( Monad m
   , l2 ~ MapMaybe l
   , is ~ Generate 0 (Length l)
   , KnownNat (Length l)
   , HTuple' (MapMaybe l) t
   , HFoldr' GetValue (Variant l, HList '[]) is (Variant l, HList l2)
   ) => m (Variant l) -> (t -> m a) -> m a
flowMatch v f = f . matchVariant =<< v


-- | Retry a flow several times on error
flowRetry :: (Monad m) => Int -> m (Variant l) -> m (Variant l)
flowRetry n f = do
   r <- f
   case (n,getVariant0 r) of
      (0,_)       -> return r
      (_,Just _)  -> return r
      (_,Nothing) -> flowRetry (n-1) f

-- | Bind during a flow
flowBind :: Monad m => m a -> (a -> m b) -> m b
flowBind = (>>=)


-- | Catch all the values of type `a`
flowCatch :: forall l a l2 b m is r.
   ( IsMember a l ~ 'True
   , Monad m
   , r ~ (Variant l, Int, Maybe Found)
   , is ~ Zip (Indexes l) (MapTest a l)
   , HFoldr' RemoveType r is r
   , l2 ~ Filter a l
   )
   => m (Variant l) -> (Either a (Variant l2) -> m b) -> m b
flowCatch v f = f . removeType =<< v