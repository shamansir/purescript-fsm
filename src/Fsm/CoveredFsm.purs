module CoveredFsm
    (CoveredFsm, make, make') where

import Prelude ((>>>), class Semigroup)

import Effect

import Control.Alt ((<|>))

import Data.Tuple.Nested (type (/\))
import Data.List (List)
import Data.Covered (Covered)
import Data.Covered (appendErrors) as Covered

import Fsm (Fsm)
import Fsm (make, joinWith) as Fsm


type CoveredFsm error action model = Fsm action (Covered error model)


make
    :: forall error action model
     . (action
            -> Covered error model
            -> Covered error model /\ List (Effect action))
    -> CoveredFsm error action model
make =
    Fsm.make
        >>> Fsm.joinWith ((<|>))

make'
    :: forall error action model
     . Semigroup error
    => (action
            -> Covered error model
            -> Covered error model /\ List (Effect action))
    -> CoveredFsm error action model
make' =
    Fsm.make
        >>> Fsm.joinWith Covered.appendErrors
