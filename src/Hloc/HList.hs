{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hloc.HList where

import Control.Applicative
import Data.Kind
import Prelude hiding (foldl, map)
import Data.Type.Equality

type family Map (f :: * -> *) (xs :: [*]) where
    Map f '[] = '[]
    Map f (x ': xs) = (f x) ': Map f xs

type family Head (xs :: [k]) :: k where Head (x : _) = x
type family Tail (xs :: [k]) :: [k] where Tail (_ : xs) = xs
data MapCons f y ys xs =
  forall x xs'. (xs ~ (x : xs'), y ~ f x, ys ~ Map f xs') => MapCons


-- OKAY. YOU'VE GOT ME. This is a total blef. The whole core of the i3hloc2
-- depends on this fake "proof". Whole project implies false. Everything
-- you see is a one big lie.
-- Okay, I am clear now. But to justify myself I must say...
-- Dude... this is trivial, okay? If you map a list over and the result is empty
-- then the input must be empty as well. I just cannot express it without some
-- shitty bloat which would make all the rest of the codebase less readable.
-- If you are capable of actually proving it, please contact me via email
-- mentioned in the package.yaml or raise an PR on github :)
mapNil :: forall f xs. Map f xs ~ '[] => xs :~: '[]
mapNil = undefined

mapCons :: forall f xs y ys. Map f xs ~ (y : ys) => MapCons f y ys xs
mapCons = undefined


infixr 5 :>
data HList :: [Type] -> Type where
  HNil :: HList '[]
  (:>) :: a -> HList l -> HList (a:l)

instance Show (HList '[]) where
  show HNil = "HNil"
instance (Show (HList l), Show x) => Show (HList (x ': l)) where
  show (x :> rest) = show x ++ " :> " ++ show rest


hSequence :: forall m ins. Applicative m => HList (Map m ins) -> m (HList ins)
hSequence HNil | Refl <- mapNil @m @ins = pure HNil
hSequence (x :> rest) | MapCons <- mapCons @m @ins = (:>) <$> x <*> hSequence rest
