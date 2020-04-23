module Fsm
    ( Fsm
    , make
    , run, fold
    , imapModel, imapAction
    , joinWith
    ) where


import Prelude

import Effect (Effect)
import Effect.Ref as Ref

import Data.List (List)
import Data.List as List
import Data.Foldable (class Foldable)
import Data.Tuple (fst) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Traversable (traverse_)
import Data.Bifunctor (bimap)
import Data.Functor.Invariant (class Invariant)

import FRP.Event (Event)
import FRP.Event as Event


data Fsm action model =
    Fsm (action -> model -> model /\ List (Effect action))


instance invariantFsm :: Invariant (Fsm action) where
    imap = imapModel


make
    :: forall action model
     . (action -> model -> model /\ List (Effect action))
    -> Fsm action model
make = Fsm


run
    :: forall action model
     . Fsm action model
    -> (model -> Effect Unit)
    -> model
    -> Effect
            { push :: action -> Effect Unit
            , stop :: Effect Unit
            }
run (Fsm updateF) subModels init = do
    { event : actions, push : pushAction } <- Event.create
    let
        (updates :: Event (model /\ List (Effect action))) =
            Event.fold
                (\action prev -> updateF action $ Tuple.fst prev)
                actions
                (init /\ List.Nil)
        (models :: Event model)
            = Tuple.fst <$> updates
    stopModelSubscription <- Event.subscribe models subModels
    stopPerformingEffects <- Event.subscribe updates
        \(_ /\ effs) -> traverse_ ((=<<) pushAction) effs
    pure
        { push : pushAction
        , stop : stopModelSubscription <> stopPerformingEffects
        }


fold
    :: forall action model f
     . Foldable f
    => Fsm action model
    -> model
    -> f action
    -> Effect model
fold fsm init actionList = do
    lastValRef <- Ref.new init
    { push, stop } <-
        run fsm (flip Ref.write lastValRef) init
    _ <- traverse_ push actionList
    lastVal <- Ref.read lastValRef
    pure lastVal


joinWith
    :: forall action model
     . (model -> model -> model)
    -> Fsm action model
    -> Fsm action model
joinWith joinF (Fsm updateF) =
    Fsm $ \action model ->
            let model' /\ effects' = updateF action model
            in (model `joinF` model') /\ effects'


imapAction
    :: forall actionA actionB model
     . (actionA -> actionB)
    -> (actionB -> actionA)
    -> Fsm actionA model
    -> Fsm actionB model
imapAction mapAToB mapBToA (Fsm updateF) =
    Fsm \actionB model ->
        map (map (map mapAToB))
            $ updateF (mapBToA actionB) model


imapModel
    :: forall action modelA modelB
     . (modelA -> modelB)
    -> (modelB -> modelA)
    -> Fsm action modelA
    -> Fsm action modelB
imapModel mapAToB mapBToA (Fsm updateF) =
    Fsm \action modelB ->
        bimap mapAToB identity $ updateF action $ mapBToA modelB
