module Example
    ( App
    , Error, Action, Model
    , init, app
    ) where

import Effect (Effect)

import Data.Tuple.Nested ((/\), type (/\))
import Data.List (List(..))
import Data.Covered (Covered)

import Spork.Html (Html)
import Spork.Html (div, text) as H

import Ui (Ui)
import Ui (make) as Ui


data Error = Error

data Action = Action

data Model = Model

type App =
    Ui Error Action Model (Html Action)


init :: Model
init = Model


update :: Action -> Covered Error Model -> Covered Error Model /\ List (Effect Action)
update action covered = covered /\ Nil


view :: Covered Error Model -> Html Action
view _ =
    H.div [] [ H.text "example" ]


app :: App
app =
    Ui.make update view


