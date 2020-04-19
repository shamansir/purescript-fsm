module Example
    ( HtmlRenderer
    , Error, Action, Model
    ) where

import Spork.Html (Html)

import Ui (Ui)

data Error = Error

data Action = Action

data Model = Model

type HtmlRenderer =
    Ui Error Action Model (Html Action)
