{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module T24934a where

-- base
import Data.Kind
  ( Type, Constraint )
import GHC.TypeNats
  ( Nat )

--------------------------------------------------------------------------------

type Fin :: Nat -> Type
newtype Fin n = Fin Word
  deriving stock ( Eq, Ord, Show )

type RepDim :: k -> Nat
type family RepDim v
type Representable :: Type -> Type -> Constraint
class Representable r v | v -> r where
  tabulate :: ( Fin ( RepDim v ) -> r ) -> v

--------------------------------------------------------------------------------

transform2 :: ( Representable r m, RepDim m ~ 2 )
          => m -> m
transform2 _ = error "not relevant"
{-# INLINEABLE transform2 #-}
