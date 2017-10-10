module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef)
import Data.Array (insertAt, delete)
import Data.Maybe (fromMaybe)
import KeyBasedDiff (Effector, Operation(..), operateDiff)
import Test.Assert (ASSERT, assert)



main :: forall e. Eff (assert :: ASSERT, ref :: REF | e) Unit
main = do
  testList [ 1, 2, 3, 4, 5 ] []
  testList [] [ 1, 2, 3, 4, 5 ]
  testList [ 1, 2, 3, 4, 5 ] [ 1, 2, 3, 4, 5 ]
  testList [ 1, 2, 3, 4, 5 ] [ 5, 4, 3, 2, 1 ]
  testList [ 1, 2, 3, 4, 5 ] [ 5, 4, 3, 2, 1, 6 ]
  testList [ 1, 2, 3, 4, 5 ] [ 0, 1, 2, 3, 4, 5 ]
  testList [ 1, 2, 3, 4, 5 ] [ 1, 2, 3, 4, 5, 6 ]
  testList [ 1, 2, 3, 4, 5 ] [ 1, 2, 3, -1, 4, 5 ]
  testList [ 1, 2, 3, 4, 5 ] [ 2, 3, 4, 5 ]
  testList [ 1, 2, 3, 4, 5 ] [ 1, 2, 3, 4 ]
  testList [ 1, 2, 3, 4, 5 ] [ 1, 2, 4, 5 ]
  testList [ 1, 2, 3, 4, 5 ] [ 2, 5, 100, 3, 99, 1, 4 ]



testList :: forall e. Array Int -> Array Int -> Eff (assert :: ASSERT, ref :: REF | e) Unit
testList prev next = do
  ref <- newRef prev
  operateDiff prev next $ operate ref
  result <- readRef ref
  assert $ result == next



operate :: forall e. Ref (Array Int) -> Effector (ref :: REF | e) Int
operate origin = \operation ->
  case operation of
    Create item idx -> modifyRef origin \l -> fromMaybe [] $ insertAt idx item l
    Update prev next -> pure unit
    Move prev next idx -> modifyRef origin $ delete prev >>> \l -> fromMaybe [] $ insertAt idx next l
    Remove item -> modifyRef origin $ delete item
