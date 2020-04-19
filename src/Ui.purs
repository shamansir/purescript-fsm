module Ui
    ( Ui
    , make, make'
    , view
    , run
    ) where

import Prelude

import Effect (Effect)

import Control.Alt ((<|>))

import Data.Tuple.Nested (type (/\))
import Data.List (List)
import Data.Covered (Covered)
import Data.Covered (appendErrors, carry) as Covered

import FRP.Event (Event)
import FRP.Event (create) as Event

import Fsm (Fsm)
import Fsm (run, make, joinWith) as Fsm


type CoveredFsm error action model = Fsm action (Covered error model)


data Ui error action model view =
    Ui (CoveredFsm error action model) (Covered error model -> view)


make
    :: forall error action model view
     . (action
            -> Covered error model
            -> Covered error model /\ List (Effect action))
    -> (Covered error model -> view)
    -> Ui error action model view
make updateF viewF =
    Ui (Fsm.make updateF # Fsm.joinWith (<|>)) viewF


make'
    :: forall error action model view
     . Semigroup error
    => (action
            -> Covered error model
            -> Covered error model /\ List (Effect action))
    -> (Covered error model -> view)
    -> Ui error action model view
make' updateF viewF =
    Ui (Fsm.make updateF
            # Fsm.joinWith Covered.appendErrors) viewF


view
    :: forall error action model view
     . Ui error action model view
    -> Covered error model
    -> view
view (Ui _ viewF) = viewF


run
    :: forall error action model view
     . Ui error action model view
    -> model
    -> Effect
        { next :: Event view
        , push :: action -> Effect Unit
        , stop :: Effect Unit
        }
run (Ui fsm viewF) model = do
    { event : views, push : pushView } <- Event.create
    { push, stop } <-
        Fsm.run fsm (pushView <<< viewF) (Covered.carry model)
    pure
        { next : views
        , push
        , stop
        }
