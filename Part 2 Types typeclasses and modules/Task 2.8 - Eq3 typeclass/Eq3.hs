module Eq3 (Eq3, (===)) where

import Bool3 (Bool3(..))
import MaybeNull (MaybeNull(..))

class Eq3 a where
  (===) :: a -> a -> Bool3

instance Eq3 Bool3 where
  (===) x y
    | x == y                    = x
    | x == Unk3 || y == Unk3    = Unk3
    | otherwise                 = False3

instance Eq3 a => Eq3 (MaybeNull a) where
  (===) (JustVal x) (JustVal y) = x === y
  (===) Null _                  = Unk3
  (===) _ Null                  = Unk3
  (===) _ _                     = Unk3
