{-# LANGUAGE NoImplicitPrelude #-}

module Std
  ( module Prelude,
    module Data.Functor,
    module Control.Applicative,
    (<#>),
  )
where

import Control.Applicative ((<|>))
import Data.Functor (void)
import Prelude

infixl 4 <#>

(<#>) :: Functor f => f a -> (a -> b) -> f b
functor <#> fn = fn <$> functor
