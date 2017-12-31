module KeyBasedDiff
  ( class HasKey, getKey
  , Operation(..)
  , Effector
  , operateDiff
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Safely (traverse_)
import Data.Array ((!!), length)
import Data.Maybe (Maybe(..), fromJust)
import Data.StrMap (StrMap, insert, lookup, delete, values, empty)
import Partial.Unsafe (unsafePartial)



data Operation a
  = Create a Int
  | Update a a Int
  | Move a a Int Int
  | Remove Int

type Effector e a = Operation a -> Eff e Unit

type Acc e a =
  { prev :: Array a
  , next :: Array a
  , effector :: Effector e a
  , prevStartIdx :: Int
  , prevEndIdx :: Int
  , nextStartIdx :: Int
  , nextEndIdx :: Int
  , prevKeyIdx :: StrMap Int
  }



operateDiff :: forall e a. HasKey a => Array a -> Array a -> Effector e a -> Eff e Unit
operateDiff prev next effector =
  let acc =
        { prev
        , next
        , effector
        , prevStartIdx : 0
        , prevEndIdx : length prev - 1
        , nextStartIdx : 0
        , nextEndIdx : length next - 1
        , prevKeyIdx : empty
        }
   in do
      acc' <- operateStandardBehavior acc
      if isFinishedNext acc'
        then operateRemovement acc'
        else pure (initPrevKeyIdx acc') >>= operateCreationAndMovement >>= operateRemovementFromKeyIdx



operateStandardBehavior :: forall e a. HasKey a => Acc e a -> Eff e (Acc e a)
operateStandardBehavior accum = tailRecM go accum
  where
    go acc
      | isFinishedPrev acc || isFinishedNext acc = pure $ Done acc
      | otherwise =
          let pStart = acc.prev !! acc.prevStartIdx
              pEnd = acc.prev !! acc.prevEndIdx
              nStart = acc.next !! acc.nextStartIdx
              nEnd = acc.next !! acc.nextEndIdx
           in case pStart, pEnd, nStart, nEnd of
                Just ps, _, Just ns, _ | isSameKey ps ns -> do
                  acc.effector $ Update ps ns acc.prevStartIdx
                  pure $ Loop $ forwardPrevStart >>> forwardNextStart $ acc
                _, Just pe, _, Just ne | isSameKey pe ne -> do
                  acc.effector $ Update pe ne acc.prevEndIdx
                  pure $ Loop $ backPrevEnd >>> backNextEnd $ acc
                Just ps, _, _, Just ne | isSameKey ps ne ->
                  let nextIdx =
                        if length acc.prev < length acc.next
                          then acc.nextEndIdx - (length acc.next - length acc.prev)
                          else acc.nextEndIdx
                   in do
                      acc.effector $ Move ps ne acc.prevStartIdx nextIdx
                      pure $ Loop $ forwardPrevStart >>> backNextEnd $ acc
                _, Just pe, Just ns, _ | isSameKey pe ns -> do
                  acc.effector $ Move pe ns acc.prevEndIdx acc.nextStartIdx
                  pure $ Loop $ backPrevEnd >>> forwardNextStart $ acc
                _, _, _, _ -> pure $ Done acc



operateRemovement :: forall e a. HasKey a => Acc e a -> Eff e Unit
operateRemovement accum = tailRecM go accum
  where
    go acc
      | isFinishedPrev acc = pure $ Done unit
      | otherwise = do
          acc.effector $ Remove acc.prevStartIdx
          pure $ Loop $ forwardPrevStart acc



operateCreationAndMovement :: forall e a. HasKey a => Acc e a -> Eff e (Acc e a)
operateCreationAndMovement accum = tailRecM go accum
  where
    go acc
      | isFinishedNext acc = pure $ Done acc
      | otherwise =
          let nStart = unsafePartial $ fromJust $ acc.next !! acc.nextStartIdx
              pStartIdx = lookup (getKey nStart) acc.prevKeyIdx
           in Loop <<< forwardNextStart <$> case pStartIdx of
                Nothing -> do
                  acc.effector $ Create nStart acc.nextStartIdx
                  pure acc
                Just idx ->
                  let pStart = unsafePartial $ fromJust $ acc.prev !! idx
                   in do
                     acc.effector $ Move pStart nStart idx acc.nextStartIdx
                     pure $ acc { prevKeyIdx = delete (getKey nStart) acc.prevKeyIdx }



operateRemovementFromKeyIdx :: forall e a. HasKey a => Acc e a -> Eff e Unit
operateRemovementFromKeyIdx acc = traverse_ (acc.effector <<< Remove) $ values acc.prevKeyIdx



initPrevKeyIdx :: forall e a. HasKey a => Acc e a -> Acc e a
initPrevKeyIdx acc
  | isFinishedPrev acc = acc
  | otherwise =
      let pStart = unsafePartial $ fromJust $ acc.prev !! acc.prevStartIdx
       in initPrevKeyIdx <<< forwardPrevStart $ acc { prevKeyIdx = insert (getKey pStart) acc.prevStartIdx acc.prevKeyIdx }



isFinishedPrev :: forall e a. Acc e a -> Boolean
isFinishedPrev acc = acc.prevStartIdx > acc.prevEndIdx



isFinishedNext :: forall e a. Acc e a -> Boolean
isFinishedNext acc = acc.nextStartIdx > acc.nextEndIdx



forwardPrevStart :: forall e a. Acc e a -> Acc e a
forwardPrevStart acc = acc { prevStartIdx = acc.prevStartIdx + 1 }



forwardNextStart :: forall e a. Acc e a -> Acc e a
forwardNextStart acc = acc { nextStartIdx = acc.nextStartIdx + 1 }



backPrevEnd :: forall e a. Acc e a -> Acc e a
backPrevEnd acc = acc { prevEndIdx = acc.prevEndIdx - 1 }



backNextEnd :: forall e a. Acc e a -> Acc e a
backNextEnd acc = acc { nextEndIdx = acc.nextEndIdx - 1 }



isSameKey :: forall a. HasKey a => a -> a -> Boolean
isSameKey p n = getKey p == getKey n



class HasKey a where
  getKey :: a -> String



instance hasKeyString :: HasKey String where
  getKey a = a



instance hasKeyInt :: HasKey Int where
  getKey a = show a
