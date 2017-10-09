module KeyBasedDiff
  ( class HasKey, getKey
  , Operation(..)
  , Effector
  , operateDiff
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.State.Trans (StateT, get, gets, modify, evalStateT)
import Data.Array ((!!), length)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, insert, lookup, delete, values, empty)



data Operation a
  = Create a Int
  | Update a a
  | Move a a Int
  | Remove a

type Effector e a = Operation a -> Eff e Unit

type InternalState e a =
  { prev :: Array a
  , next :: Array a
  , effector :: Effector e a
  , prevStartIdx :: Int
  , prevEndIdx :: Int
  , nextStartIdx :: Int
  , nextEndIdx :: Int
  , prevKeyValues :: StrMap a
  }



operateDiff :: forall e a. HasKey a => Array a -> Array a -> Effector e a -> Eff e Unit
operateDiff prev next effector =
  let state =
        { prev
        , next
        , effector
        , prevStartIdx : 0
        , prevEndIdx : length prev
        , nextStartIdx : 0
        , nextEndIdx : length next
        , prevKeyValues : empty
        }
   in flip evalStateT state do
      operateStandardBehavior
      operateCreationAndMovement
      operateRemovement



operateStandardBehavior :: forall e a. HasKey a => StateT (InternalState e a) (Eff e) Unit
operateStandardBehavior = do
  state <- get
  pStart <- gets \s -> s.prev !! s.prevStartIdx
  pEnd <- gets \s -> s.prev !! s.prevEndIdx
  nStart <- gets \s -> s.next !! s.nextStartIdx
  nEnd <- gets \s -> s.next !! s.nextEndIdx
  case pStart, pEnd, nStart, nEnd of
    Nothing, _, _, _ -> do
      modify forwardPrevStart
      when (isntFinishedAny state) operateStandardBehavior

    _, Nothing, _, _ -> do
      modify backPrevEnd
      when (isntFinishedAny state) operateStandardBehavior

    Just ps, _, Just ns, _ | isSameKey ps ns -> do
      liftEff $ state.effector $ Update ps ns
      modify $ forwardPrevStart >>> forwardNextStart
      when (isntFinishedAny state) operateStandardBehavior

    _, Just pe, _, Just ne | isSameKey pe ne -> do
      liftEff $ state.effector $ Update pe ne
      modify $ backPrevEnd >>> backNextEnd
      when (isntFinishedAny state) operateStandardBehavior

    Just ps, _, _, Just ne | isSameKey ps ne -> do
      liftEff $ state.effector $ Move ps ne state.nextEndIdx
      modify $ forwardPrevStart >>> backNextEnd
      when (isntFinishedAny state) operateStandardBehavior

    _, Just pe, Just ns, _ | isSameKey pe ns -> do
      liftEff $ state.effector $ Move pe ns state.nextStartIdx
      modify $ backPrevEnd >>> forwardNextStart
      when (isntFinishedAny state) operateStandardBehavior

    _, _, _, _ -> pure unit



operateCreationAndMovement :: forall e a. HasKey a => StateT (InternalState e a) (Eff e) Unit
operateCreationAndMovement = do
  modify initPrevKeyValues
  state <- get
  nStart <- gets \s -> s.next !! s.nextStartIdx
  case nStart of
    Nothing -> pure unit
    Just ns -> do
      pStart <- gets \s -> lookup (getKey ns) s.prevKeyValues
      case pStart of
        Nothing -> do
          liftEff $ state.effector $ Create ns state.nextStartIdx
          modify forwardNextStart
          when (isntFinishedNext state) operateCreationAndMovement
        Just ps -> do
          liftEff $ state.effector $ Move ps ns state.nextStartIdx
          modify forwardNextStart
          modify \s -> s { prevKeyValues = delete (getKey ns) s.prevKeyValues }
          when (isntFinishedNext state) operateCreationAndMovement



operateRemovement :: forall e a. StateT (InternalState e a) (Eff e) Unit
operateRemovement = do
  state <- get
  prevItems <- gets $ _.prevKeyValues >>> values
  liftEff $ traverse_ (state.effector <<< Remove) prevItems



initPrevKeyValues :: forall e a. HasKey a => InternalState e a -> InternalState e a
initPrevKeyValues state@{ prev, prevStartIdx, prevEndIdx }
  | prevStartIdx <= prevEndIdx =
    case prev !! prevStartIdx of
      Nothing -> forwardPrevStart state
      Just item -> initPrevKeyValues <<< forwardPrevStart $ state { prevKeyValues = insert (getKey item) item state.prevKeyValues}
  | otherwise = state



isntFinishedAny :: forall e a. InternalState e a -> Boolean
isntFinishedAny state = state.prevStartIdx <= state.prevEndIdx && isntFinishedNext state



isntFinishedNext :: forall e a. InternalState e a -> Boolean
isntFinishedNext state = state.nextStartIdx <= state.nextEndIdx



forwardPrevStart :: forall e a. InternalState e a -> InternalState e a
forwardPrevStart state = state { prevStartIdx = state.prevStartIdx + 1 }



forwardNextStart :: forall e a. InternalState e a -> InternalState e a
forwardNextStart state = state { nextStartIdx = state.nextStartIdx + 1 }



backPrevEnd :: forall e a. InternalState e a -> InternalState e a
backPrevEnd state = state { prevEndIdx = state.prevEndIdx - 1 }



backNextEnd :: forall e a. InternalState e a -> InternalState e a
backNextEnd state = state { nextEndIdx = state.nextEndIdx - 1 }



isSameKey :: forall a. HasKey a => a -> a -> Boolean
isSameKey p n = getKey p == getKey n



class HasKey a where
  getKey :: a -> String



instance hasKeyString :: HasKey String where
  getKey a = a



instance hasKeyInt :: HasKey Int where
  getKey a = show a
