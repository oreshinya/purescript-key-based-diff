module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef)
import Data.Array ((!!), insertAt, delete, snoc)
import Data.Maybe (Maybe(..), fromMaybe)
import KeyBasedDiff (Effector, Operation(..), operateDiff)
import Test.Assert (ASSERT, assert)



main :: forall e. Eff (assert :: ASSERT, ref :: REF | e) Unit
main = do
  testList [ 1, 2, 3, 4, 5 ] [ 1, 2, 3, 4, 5 ]
  testList [ 1, 2, 3, 4, 5 ] [ 5, 4, 3, 2, 1 ]
  testList [ 1, 2, 3, 4, 5 ] [ 8, 0, 1, 2, 3, 4, 5 ]
  testList [ 1, 2, 3, 4, 5 ] [ 1, 2, 8, 0, 3, 4, 5 ]
  testList [ 1, 2, 3, 4, 5 ] [ 1, 2, 3, 4, 5, 6, 7 ]
  testList [ 1, 2, 3, 4, 5 ] [ 8, 0, 5, 4, 3, 2, 1 ]
  testList [ 1, 2, 3, 4, 5 ] [ 5, 4, 3, 8, 0, 2, 1 ]
  testList [ 1, 2, 3, 4, 5 ] [ 5, 4, 3, 2, 1, 0, 8 ]
  testList [ 1, 2, 3, 4, 5 ] [ 2, 3, 4, 5 ]
  testList [ 1, 2, 3, 4, 5 ] [ 1, 2, 4, 5 ]
  testList [ 1, 2, 3, 4, 5 ] [ 1, 2, 3, 4 ]
  testList [ 1, 2, 3, 4, 5 ] [ 4, 3, 2, 1 ]
  testList [ 1, 2, 3, 4, 5 ] [ 5, 4, 2, 1 ]
  testList [ 1, 2, 3, 4, 5 ] [ 5, 4, 3, 2 ]
  testList [ 1, 2, 3, 4, 5 ] []
  testList [] [ 1, 2, 3, 4, 5 ]
  testList [ 1, 2, 3, 4, 5 ] [ 2, 5, 100, 3, 99, 1, 4 ]



testList :: forall e. Array Int -> Array Int -> Eff (assert :: ASSERT, ref :: REF | e) Unit
testList prev next = do
  ref <- newRef prev
  operateDiff prev next $ operate ref prev next
  result <- readRef ref
  assert $ result == next



operate :: forall e. Ref (Array Int) -> Array Int -> Array Int -> Effector (ref :: REF | e) Int
operate origin prev next = \operation ->
  case operation of
    Create item idx ->
      modifyRef origin \l -> fromMaybe [] $ insertAt idx item l

    Move prevItem nextItem _ nextIdx -> do
      modifyRef origin $ delete prevItem
      currentList <- readRef origin
      modifyRef origin \l ->
        case insertAt nextIdx nextItem l of
          Just l' -> l'
          Nothing -> snoc l nextItem

    Remove idx ->
      case prev !! idx of
        Just item -> modifyRef origin $ delete item
        _ -> pure unit

    _ -> pure unit
