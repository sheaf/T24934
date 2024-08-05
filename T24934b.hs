{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module T24934b
  ( I(..), IR(..), C(..), Cross(..)
  , runMainComputation
  )
  where

-- base
import Data.Coerce
  ( coerce )
import Data.Kind
  ( Type )
import GHC.TypeNats
  ( Nat )

-- T24934
import T24934a
  ( RepDim, Representable(..)
  , Fin(..)
  , transform2
  )

--------------------------------------------------------------------------------

type R :: Nat -> Type
data family R n

newtype instance R 1 = R1 { unR1 :: Double }
  deriving stock   Show
  deriving newtype ( Eq, Ord )
data    instance R 2 = R2 { _R2_x, _R2_y :: Double }
  deriving stock    ( Eq, Ord, Show )

type instance RepDim ( R n ) = n
instance Representable Double ( R 2 ) where
  tabulate f = R2 ( f ( Fin 1 ) ) ( f ( Fin 2 ) )

--------------------------------------------------------------------------------

type C :: Nat -> Type -> Type -> Type
newtype C k u v = D { runD :: u -> D k u v }

type D :: Nat -> Type -> Type -> Type
type family D k u
type instance D 3 ( R 1 ) = D3
type instance D 3 ( R 2 ) = D3
type instance D k I = D k Double
type instance D k ( IR n ) = D k ( R n )

data D3 v =
  D3 { _D3_v :: v }
  deriving stock Functor
instance Applicative D3 where
  pure = D3
  D3 f <*> D3 a = D3 ( f a )

--------------------------------------------------------------------------------

data I = I { inf, sup :: Double }
  deriving stock ( Show, Eq )

type IR :: Nat -> Type
data family IR n

newtype instance IR 1 = IR1 { unIR1 :: I }
  deriving stock   Show
  deriving newtype Eq
data    instance IR 2 = IR2 { _IR2_x, _IR2_y :: !I }
  deriving stock ( Show, Eq )

type instance RepDim ( IR n ) = n
instance Representable I ( IR 2 ) where
  tabulate f = IR2 ( f ( Fin 1 ) ) ( f ( Fin 2 ) )

--------------------------------------------------------------------------------

type F :: k -> l -> Type
type family F i t
type instance F @(Nat -> Type) @Type R Double = Double
type instance F @Type          @Type I Double = I
type instance F @(Nat -> Type) @Nat  R n      = R n
type instance F @Type          @Nat  I n      = IR n

runMainComputation :: forall (k :: Nat)
                   .  ( HasMainComputation k
                      , Applicative ( D k ( R 2 ) )
                      )
                   => C k ( IR 1 ) ( IR 2 )
                   -> IR 1
                   -> IR 1
                   -> ( IR 2, IR 1 )
runMainComputation path t =
  let
    dpath_t :: D k ( IR 1 ) ( IR 2 )
    dpath_t = runD path t
  in ( \ s -> mainComputation @k @I coerce dpath_t ( pure $ tabulate $ \ _ -> coerce s ) False )
{-# INLINEABLE runMainComputation #-}

class HasMainComputation k where
  mainComputation :: forall i
                  .  ( D k ( F i 2 ) ~ D k ( R 2 )
                     , D k ( F i 1 ) ~ D k ( R 1 )
                     , Cross ( F i Double ) ( F i 2 )
                     , Representable ( F i Double ) ( F i 2 )
                     , RepDim ( F i 2 ) ~ 2
                     )
                  => ( F i Double -> F i 1 )
                  -> D k ( F i 1 ) ( F i 2 )
                  -> D k ( F i 2 ) ( F i 2 )
                  -> Bool
                  -> ( F i 2, F i 1 )

instance HasMainComputation 3 where
  mainComputation co
    ( D3 { _D3_v = p } )
    ( D3 {} )
    doTransform =
      ( case doTransform of
          False -> p
          True  -> transform2 p
      , co ( p × p ) )
  {-# INLINEABLE mainComputation #-}

--------------------------------------------------------------------------------

class Cross r m where
  (×) :: m -> m -> r
instance Cross I ( IR 2 ) where
  (×) = error "not relevant"
