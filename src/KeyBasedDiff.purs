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
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (StrMap, insert, lookup, delete, values, empty)



data Operation
  = Create Int
  | Update Int Int
  | Move Int Int
  | ReverseAtTailBeforeCreate Int Int
  | Remove Int

type Effector e = Operation -> Eff e Unit

type InternalState e a =
  { prev :: Array a
  , next :: Array a
  , effector :: Effector e
  , prevStartIdx :: Int
  , prevEndIdx :: Int
  , nextStartIdx :: Int
  , nextEndIdx :: Int
  , prevKeyIdx :: StrMap Int
  }



operateDiff :: forall e a. HasKey a => Array a -> Array a -> Effector e -> Eff e Unit
operateDiff prev next effector =
  let state =
        { prev
        , next
        , effector
        , prevStartIdx : 0
        , prevEndIdx : length prev - 1
        , nextStartIdx : 0
        , nextEndIdx : length next - 1
        , prevKeyIdx : empty
        }
   in flip evalStateT state do
      operateStandardBehavior
      modify initPrevKeyIdx
      operateCreationAndMovement
      operateRemovement



operateStandardBehavior :: forall e a. HasKey a => StateT (InternalState e a) (Eff e) Unit
operateStandardBehavior = do
  state <- get
  when (isntFinishedAny state) do
    pStart <- gets \s -> s.prev !! s.prevStartIdx
    pEnd <- gets \s -> s.prev !! s.prevEndIdx
    nStart <- gets \s -> s.next !! s.nextStartIdx
    nEnd <- gets \s -> s.next !! s.nextEndIdx
    case pStart, pEnd, nStart, nEnd of
      Just ps, _, Just ns, _ | isSameKey ps ns -> do
        liftEff $ state.effector $ Update state.prevStartIdx state.nextStartIdx
        modify $ forwardPrevStart >>> forwardNextStart
        operateStandardBehavior

      _, Just pe, _, Just ne | isSameKey pe ne -> do
        liftEff $ state.effector $ Update state.prevEndIdx state.nextEndIdx
        modify $ backPrevEnd >>> backNextEnd
        operateStandardBehavior

      Just ps, _, _, Just ne | isSameKey ps ne ->
        let operation = if length state.prev < length state.next then ReverseAtTailBeforeCreate else Move
         in do
            liftEff $ state.effector $ operation state.prevStartIdx state.nextEndIdx
            modify $ forwardPrevStart >>> backNextEnd
            operateStandardBehavior

      _, Just pe, Just ns, _ | isSameKey pe ns -> do
        liftEff $ state.effector $ Move state.prevEndIdx state.nextStartIdx
        modify $ backPrevEnd >>> forwardNextStart
        operateStandardBehavior

      _, _, _, _ -> pure unit



operateCreationAndMovement :: forall e a. HasKey a => StateT (InternalState e a) (Eff e) Unit
operateCreationAndMovement = do
  state <- get
  when (isntFinishedNext state) do
    nStart <- gets \s -> s.next !! s.nextStartIdx
    flip (maybe $ pure unit) nStart \ns -> do
      pStartIdx <- gets $ _.prevKeyIdx >>> (lookup $ getKey ns)
      case pStartIdx of
        Nothing -> liftEff $ state.effector $ Create state.nextStartIdx
        Just idx -> do
          liftEff $ state.effector $ Move idx state.nextStartIdx
          modify \s -> s { prevKeyIdx = delete (getKey ns) s.prevKeyIdx }
    modify forwardNextStart
    operateCreationAndMovement



operateRemovement :: forall e a. StateT (InternalState e a) (Eff e) Unit
operateRemovement = do
  state <- get
  indexes <- gets $ _.prevKeyIdx >>> values
  liftEff $ traverse_ (state.effector <<< Remove) indexes



initPrevKeyIdx :: forall e a. HasKey a => InternalState e a -> InternalState e a
initPrevKeyIdx state
  | isntFinishedPrev state = initPrevKeyIdx <<< forwardPrevStart $
    case state.prev !! state.prevStartIdx of
      Nothing -> state
      Just item -> state { prevKeyIdx = insert (getKey item) state.prevStartIdx state.prevKeyIdx }
  | otherwise = state



isntFinishedAny :: forall e a. InternalState e a -> Boolean
isntFinishedAny state = isntFinishedPrev state && isntFinishedNext state



isntFinishedPrev :: forall e a. InternalState e a -> Boolean
isntFinishedPrev state = state.prevStartIdx <= state.prevEndIdx



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
