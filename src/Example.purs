module Example
    ( App
    , Error, Action, Model
    , init, app

    , Host, Species, MuseumFacade
    , Bug, Fish, SkeletonPart
    ) where

import Effect (Effect)

import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\), type (/\))
import Data.List (List(..))
import Data.Covered (Covered)

import Spork.Html (Html)
import Spork.Html (div, text) as H

import Ui (Ui)
import Ui (make) as Ui


data SkeletonPart
    = Jaws
    | Tail
    | Backbone
    | Skull
    | Ribs
    | Neck


data Fossil = Fossil SkeletonPart

data Bug
    = Butterfly
    | Spider
    | Ladybug
    | Caterpillar


data Fish
    = Bass
    | Peackock


type Species =
    { bugs :: List Bug
    , fish :: List Fish
    , skeletons :: List SkeletonPart
    }


type Location = Int /\ Int


data MuseumFacade
    = Nowhere
    | Tent Location
    | Building Location


data Host
    = TomNook
    | Blanthers


type Model =
    { museum ::
        { host :: Host
        , exposition :: Species
        , facade :: MuseumFacade
        }
    , player :: Species
    }


data PlayerAction
    = Dig
    | Hook
    | Catch
    | GetFish Fish
    | GetBug Bug
    | GetFossil Fossil
    | GetNoFish
    | GetNoBug
    | GetNoFossil
    | Deliver Species


data HostAction
    = AcceptFish Fish
    | AcceptBug Bug
    | AcceptSkeletonPart SkeletonPart
    | DenyFish Fish
    | DenyBug Bug
    | DentSkeletonPart SkeletonPart
    | ConsiderSpecies Species
    | UnpackFossil Fossil
    | SpeakDetails String
    | InformLackOfAmount Int
    | RequireTentLocation
    | RequireMuseumLocation


data Action
    = ByPlayer PlayerAction
    | ByHost HostAction
    | ProduceError Int


data Error = Error Int


type App =
    Ui Error Action Model (Html Action)


noSpecies :: Species
noSpecies =
    { fish : Nil
    , bugs : Nil
    , skeletons : Nil
    }


init :: Model
init =
    { museum :
        { host : TomNook
        , exposition : noSpecies
        , facade : Nowhere
        }
    , player : noSpecies
    }


update :: Action -> Covered Error Model -> Covered Error Model /\ List (Effect Action)
update action covered = covered /\ Nil


view :: Covered Error Model -> Html Action
view _ =
    H.div [] [ H.text "example" ]


app :: App
app =
    Ui.make update view


