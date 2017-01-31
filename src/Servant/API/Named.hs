{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}

module Servant.API.Named (
  Named(..),
  NameList(..),
  (::=)(..),
  Nil,
  (:=),
  (::<|>),
  FromNamed,
  fromNamed
  ) where


import Servant.API.Named.Internal

import Data.Kind (Type)

import GHC.TypeLits (Symbol, CmpSymbol)


type Nil = '[]

infix 8 :=


-- | Notational convenience to make value and type level defns match up
type family n := a where
   (Named n) := a = '(n, a)

infixr 7 ::<|>

-- | Notational convenience to make value and type level defns match up
type family x ::<|> y where
   x ::<|> y = x ': y


type family Insert (x :: (Symbol, Type)) (xs :: [(Symbol, Type)]) :: [(Symbol, Type)] where
  Insert x '[] = '[x]
  Insert '(xName, x) ( '(yName, y) ': ys) = If ((CmpSymbol xName yName) == 'LT)
                                               ( '(xName, x) ': '(yName, y) ': ys )
                                               ( '(yName, y) ': Insert '(xName, x) ys )


type family Sort (xs :: [(Symbol, Type)]) :: [(Symbol, Type)] where
  Sort '[] = '[]
  Sort (x ': xs) = Insert x (Sort xs)


sInsert :: Named name -> a -> NameList xs -> NameList (Insert '(name, a) xs)
sInsert pName val Nil = pName := val ::<|> Nil
sInsert xName xVal xs@(yName := yVal ::<|> rest) = sIf (sCompare xName yName) (xName := xVal ::<|> xs) (yName := yVal ::<|> (sInsert xName xVal rest))


sSort :: NameList xs -> NameList (Sort xs)
sSort Nil = Nil
sSort (name := val ::<|> rest) = sInsert name val (sSort rest)


type family FromNamed xs where
  FromNamed xs = FromNamedSorted (Sort xs)

fromNamed :: NameList xs -> FromNamed xs
fromNamed xs = fromNamedSorted (sSort xs)
