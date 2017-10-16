module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef)
import Data.Array ((!!), insertAt, delete)
import Data.Maybe (Maybe(..), fromMaybe)
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
  operateDiff prev next $ operate ref prev next
  result <- readRef ref
  assert $ result == next



operate :: forall e. Ref (Array Int) -> Array Int -> Array Int -> Effector (ref :: REF | e)
operate origin prev next = \operation ->
  case operation of
    Create idx ->
      case next !! idx of
        Just item ->
          modifyRef origin \l -> fromMaybe [] $ insertAt idx item l
        _ -> pure unit

    Update prevIdx nextIdx ->
      case prev !! prevIdx, next !! nextIdx of
        Just prevItem, Just nextItem ->
          modifyRef origin $ delete prevItem >>> \l -> fromMaybe [] $ insertAt nextIdx nextItem l
        _, _ -> pure unit

    Remove idx ->
      case prev !! idx of
        Just item -> modifyRef origin $ delete item
        _ -> pure unit
