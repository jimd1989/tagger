module Helpers ((◁), (◀), (⊙), (●), (◇), (≠), fork) where

import Protolude (Bool, (.), (/=), (<=<), (<>), fmap)
import Control.Applicative (Alternative, Applicative, (<*>), liftA2)

fork :: Applicative f ⇒ (a → b → c) → f a → f b → f c
fork = liftA2

-- Digraph Tl
f ◁ g = fmap f . g
infixr 9 ◁

-- Digraph PL
f ◀ g = f <=< g
infixr 1 ◀

-- Digraph 0.
f ⊙ g = fmap f g
infixl 4 ⊙

-- Digraph 0M
f ● g = f <*> g
infixl 4 ●

-- Digraph Dw
α ◇ ω = α <> ω
infixr 5 ◇

-- Digraph !=
α ≠ ω = α /= ω
infix 4 ≠
