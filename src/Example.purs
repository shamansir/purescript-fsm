module Example
    ( App
    , Error, Action, Model
    , init, app

    , Host, Species, MuseumFacade
    , Bug, Fish, SkeletonPart, Fossil
    ) where

import Prelude

import Effect (Effect)
import Effect.Random (random) as Random

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.List (List(..), (:))
import Data.List (union, difference) as List
import Data.Covered (Covered(..))
import Data.Covered (note) as Covered

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
    | Tarantula


data Fish
    = Bass
    | Peacock


type Species =
    { bugs :: List Bug
    , fish :: List Fish
    , skeletons :: List SkeletonPart
    , fossils :: List Fossil
    }


derive instance eqSkeletonPart :: Eq SkeletonPart
derive instance eqFossil :: Eq Fossil
derive instance eqBug :: Eq Bug
derive instance eqFish :: Eq Fish


addSkeleton species skeletonPart = species { skeletons = skeletonPart : species.skeletons }
addFossil species fossil = species { fossils = fossil : species.fossils }
addBug species bug = species { bugs = bug : species.bugs }
addFish species fish = species { fish = fish : species.fish }

partsChoice =
    (Jaws /\ 0.3) : (Tail /\ 0.4) : (Neck /\ 0.5) : (Ribs /\ 0.6) : (Skull /\ 0.8)
    : Nil
bugsChoice =
    (Butterfly /\ 0.3) : (Spider /\ 0.5) : (Ladybug /\ 0.6) : (Caterpillar /\ 0.8)
    : Nil


type Location = Number /\ Number


data MuseumFacade
    = Nowhere
    | Tent Location
    | Building Location


data Host
    = TomNook
    | Blathers


type Model =
    { museum ::
        { host :: Host
        , exposition :: Species
        , facade :: MuseumFacade
        , open :: Boolean
        , speech :: Maybe String
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
    | FindMuseumSpot
    | LocateMuseumSpot Location
    | Deliver Species
    | DeliverFossil Fossil


data HostAction
    = ConsiderSpecies Species
    | ConsiderFossil Fossil
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
    , fossils : Nil
    }


init :: Model
init =
    { museum :
        { host : TomNook
        , exposition : noSpecies
        , facade : Nowhere
        , open : true
        , speech : Nothing
        }
    , player : noSpecies
    }


update :: Action -> Model -> Either Error Model /\ List (Effect Action)

update (ByPlayer playerAction) model = playerUpdate playerAction where

    decide :: forall a. a -> a -> Number -> Number -> a
    decide vA _ p n | n < p     = vA
    decide _ vB p n | otherwise = vB

    decide' :: forall a. List ( a /\ Number ) -> a -> Number -> a
    decide' Nil fallback _ = fallback
    decide' ((v /\ p) : vs) fallback n | p <= n    = v
    decide' (       _ : vs) fallback n | otherwise = decide' vs fallback n

    playerUpdate Dig =
        pure model
        /\ do
            n1 <- Random.random
            n2 <- Random.random
            pure $ ByPlayer
                 $ decide
                    (GetFossil $ Fossil $ decide' partsChoice Backbone n2)
                    GetNoFossil 0.4 n1
            : Nil

    playerUpdate Hook =
        pure model
        /\ do
            n1 <- Random.random
            n2 <- Random.random
            pure $ ByPlayer
                 $ decide
                    (GetBug $ decide' bugsChoice Tarantula n2)
                    GetNoBug 0.4 n1
            : Nil

    playerUpdate Catch =
        pure model
        /\ do
            n1 <- Random.random
            n2 <- Random.random
            pure $ ByPlayer
                 $ decide (GetFish $ decide Bass Peacock 0.4 n2) GetNoFish 0.4 n1
            : Nil

    playerUpdate (GetFish fish) =
        pure model
            { player = fish # addFish model.player
            }
        /\ Nil

    playerUpdate (GetBug bug) =
        pure model
            { player = bug # addBug model.player
            }
        /\ Nil

    playerUpdate (GetFossil fossil) =
        pure model
            { player = fossil # addFossil model.player
            }
        /\ Nil

    playerUpdate (Deliver species) =
        pure model
        /\  (pure $ ByHost $ ConsiderSpecies species)
            : Nil

    playerUpdate FindMuseumSpot =
        pure model
        /\  (ByPlayer <<< LocateMuseumSpot
                <$> ((/\) <$> Random.random <*> Random.random)
            ) : Nil


    playerUpdate (LocateMuseumSpot location) =
        pure model
            { museum
                { facade =
                    case model.museum.facade of
                        Nowhere -> Tent location
                        Tent _ -> Building location
                        Building _ -> Building location
                , open = true
                }
            }
        /\ Nil

    playerUpdate _ = pure model /\ Nil

update (ByHost hostAction) model = hostUpdate hostAction where

    addUniqueSpecies :: Species -> Species -> Species
    addUniqueSpecies to from =
        { fish : List.union from.fish to.fish
        , bugs : List.union from.bugs to.bugs
        , skeletons : to.skeletons
        , fossils : to.fossils
        }

    findDifferences :: Species -> Species -> Species
    findDifferences speciesA speciesB =
        { fish : List.difference speciesA.fish speciesB.fish
        , bugs : List.difference speciesA.bugs speciesB.bugs
        , skeletons : List.difference speciesA.skeletons speciesB.skeletons
        , fossils : List.difference speciesA.fossils speciesB.fossils
        }

    hostUpdate (ConsiderSpecies species) =
        pure model
            { museum
                { exposition =
                    species # addUniqueSpecies model.museum.exposition
                }
            }
        /\ Nil

    hostUpdate _ = pure model /\ Nil

update (ProduceError num) _ =
    Left (Error num) /\ Nil


update' :: Action -> Covered Error Model -> Covered Error Model /\ List (Effect Action)
update' action covered = covered /\ Nil


view :: Model -> Html Action
view _ =
    H.div [] [ H.text "example" ]


view' :: Covered Error Model -> Html Action
view' covered =
    case covered of
        Carried model -> view model
        Recovered (Error n) model ->
            H.div
                [ ]
                [ view model
                , H.text $ "Error: " <> show n
                ]


app :: App
app =
    Ui.make update' view'


