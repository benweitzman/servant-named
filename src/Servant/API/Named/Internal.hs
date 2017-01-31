{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.API.Named.Internal where

import Data.Kind (Type)

import Data.List (intersperse)

import Data.Proxy (Proxy(..))

import GHC.TypeLits (Symbol, CmpSymbol, KnownSymbol, symbolVal
                    ,TypeError, ErrorMessage(Text))

import Unsafe.Coerce (unsafeCoerce)

import Servant.API ((:<|>)(..))



-- | Symbol singleton
data Named (s :: Symbol) = KnownSymbol s => Named

instance Show (Named s) where
  show Named = "Named @" ++ show (symbolVal (Proxy @s))

-- | Boolean singleton
data SBool :: Bool -> Type where
  STrue :: SBool 'True
  SFalse :: SBool 'False

deriving instance Show (SBool k)

type family (x :: k) == (y :: k) :: Bool where
  a == a = 'True
  a == b = 'False

type family If (cond :: Bool) (yes :: k) (no :: k) :: k where
  If 'True yes no = yes
  If 'False yes no = no


sIf :: SBool cond -> f a -> f b -> f (If cond a b)
sIf STrue a _ = a
sIf SFalse _ b = b


-- | Singleton version of `compare`. Requires unsafeCoerce
sCompare :: forall (a :: Symbol) (b :: Symbol) . Named a -> Named b -> SBool (CmpSymbol a b == 'LT)
sCompare Named Named = case symbolVal (Proxy @a) `compare` symbolVal (Proxy @b) of
  LT -> unsafeCoerce STrue
  EQ -> unsafeCoerce SFalse
  GT -> unsafeCoerce SFalse



data name ::= a = Named name := a


infixr 7 ::<|>

-- | Hetereogenous list with name tag
data NameList :: [(Symbol, Type)] -> Type where
  Nil :: NameList '[]
  (::<|>) :: (name ::= a) -> NameList xs -> NameList ( '(name, a) ': xs)


class ShowList x where
  toStringList :: x -> [String]

instance ShowList (NameList '[]) where
  toStringList Nil = []

instance (ShowList (NameList as), Show a) => ShowList (NameList ( '(name, a) ': as)) where
  toStringList (name := a ::<|> rest) = ("(" ++ show name ++ ", " ++ show a ++ ")") : toStringList rest


instance ShowList (NameList a) => Show (NameList a) where
  show = (++ "]") . ('[' :) . concat . intersperse  "," . toStringList



type family FromNamedSorted xs where
  FromNamedSorted '[] = TypeError ( 'Text "You can't have an empty API")
  FromNamedSorted '[ ' (_, a) ] = a
  FromNamedSorted ( '(_, a) ': rest ) = a :<|> FromNamedSorted rest

-- | Convert a sorted list of handlers in to a tuple built with :<|>.
-- requires unsafeCoerce
fromNamedSorted :: NameList xs -> FromNamedSorted xs
fromNamedSorted Nil = error "This is impossible and will generate a type error"
fromNamedSorted (_ := a ::<|> Nil) = a
fromNamedSorted (_ := a ::<|> rest) = unsafeCoerce $ a :<|> fromNamedSorted rest
