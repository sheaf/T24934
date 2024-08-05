{-# LANGUAGE DataKinds #-}

module Main where

-- T24934
import T24934b
  ( I(..), IR(..)
  , runMainComputation
  )

--------------------------------------------------------------------------------

main :: IO ()
main =
  print $
    runMainComputation @3
      ( error "we don't even get to here" )
      ( IR1 ( I 0 1 ) )
      ( IR1 ( I 0 1 ) )
