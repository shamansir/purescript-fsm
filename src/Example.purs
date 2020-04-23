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

import Data.Int (floor)
import Data.String (joinWith) as String
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\), type (/\))
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty as NEList
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Covered (Covered(..))
import Data.Covered (recover, cover) as Covered

import Spork.Html (Html)
import Spork.Html as H

import Ui (Ui)
import Ui (make, make') as Ui


data SkeletonPart
    = Jaws
    | Tail
    | Backbone
    | Skull
    | Ribs
    | Neck

newtype Fossil = Fossil SkeletonPart

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


newtype Location = Location (Number /\ Number)


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
    | GoFishing
    | Catch
    | GetFish Fish
    | GetBug Bug
    | GetFossil Fossil
    | GetNoFish
    | GetNoBug
    | GetNoFossil
    | FindMuseumSpot
    | LocateMuseumSpot Location
    | Deliver
    | GetInReturn Species
    | DeliverFossils


data HostAction
    = ConsiderSpecies Species
    | ConsiderFossils (List Fossil)


data Action
    = Player PlayerAction
    | Host HostAction
    | ProduceError


data Error
    = ProducedError
    | NoLocatingAllowed
    | NoSpeciesDeliveryAllowed
    | NoFossilsDeliveryAllowed
    | SeveralErrors (List Error)


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
        , speech : Just
            $ "Go dig and catch! We need "
                <> show upgradeToTentAt <> " unique species to upgrade to tent."
        }
    , player : noSpecies
    }


upgradeToTentAt :: Int
upgradeToTentAt = 5


upgradeToBuildingAt :: Int
upgradeToBuildingAt = 10


canDeliver :: forall a. { open :: Boolean | a } -> Boolean
canDeliver museum = museum.open
canDeliverFossils :: forall a. { open :: Boolean, facade :: MuseumFacade | a } -> Boolean
canDeliverFossils museum = museum.open && museum.facade /= Nowhere
canConsider :: forall a. { open :: Boolean | a } -> Boolean
canConsider = canDeliver
canConsiderFossils :: forall a. { open :: Boolean, facade :: MuseumFacade | a } -> Boolean
canConsiderFossils = canDeliverFossils


partsChoice :: List (SkeletonPart /\ Number)
partsChoice =
    (Jaws /\ 0.36) : (Tail /\ 0.26) : (Neck /\ 0.20) : (Ribs /\ 0.09) : (Skull /\ 0.05) : (Backbone /\ 0.04)
    : Nil


bugsChoice :: List (Bug /\ Number)
bugsChoice =
    (Butterfly /\ 0.40) : (Spider /\ 0.30) : (Ladybug /\ 0.15) : (Caterpillar /\ 0.10) : (Tarantula /\ 0.05)
    : Nil


update :: Action -> Model -> Covered Error Model /\ List (Effect Action)

update (Player playerAction) model = playerUpdate playerAction where

    decide :: forall a. a -> a -> Number -> Number -> a
    decide vA _ p n | n <= p    = vA
    decide _ vB p n | otherwise = vB

    decide' :: forall a. List ( a /\ Number ) -> a -> Number -> a
    decide' Nil fallback _ = fallback
    decide' ((v /\ p) : vs) fallback n
        | n <= p    = v
        | otherwise = decide' vs fallback $ n - p

    playerUpdate Dig =
        pure model
        /\ do
            n1 <- Random.random
            n2 <- Random.random
            pure $ Player
                 $ decide
                    (GetFossil $ Fossil $ decide' partsChoice Backbone n2)
                    GetNoFossil 0.4 n1
            : Nil

    playerUpdate Catch =
        pure model
        /\ do
            n1 <- Random.random
            n2 <- Random.random
            pure $ Player
                 $ decide
                    (GetBug $ decide' bugsChoice Tarantula n2)
                    GetNoBug 0.4 n1
            : Nil

    playerUpdate GoFishing =
        pure model
        /\ do
            n1 <- Random.random
            n2 <- Random.random
            pure $ Player
                 $ decide (GetFish $ decide Bass Peacock 0.7 n2) GetNoFish 0.4 n1
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

    playerUpdate GetNoFish = pure model /\ Nil

    playerUpdate GetNoBug = pure model /\ Nil

    playerUpdate GetNoFossil = pure model /\ Nil

    playerUpdate Deliver | canDeliver model.museum =
        pure model { player = noSpecies { fossils = model.player.fossils } }
        /\  (pure $ Host $ ConsiderSpecies model.player { fossils = Nil } )
            : Nil

    playerUpdate Deliver | otherwise =
        (NoSpeciesDeliveryAllowed # Covered.cover model) /\ Nil

    playerUpdate DeliverFossils | canDeliverFossils model.museum =
        pure model { player { fossils = Nil } }
        /\  (pure $ Host $ ConsiderFossils model.player.fossils )
            : Nil

    playerUpdate DeliverFossils | otherwise =
        (NoFossilsDeliveryAllowed # Covered.cover model) /\ Nil

    playerUpdate (GetInReturn leftovers) =
        pure model { player = addSpecies model.player leftovers }
        /\ Nil

    playerUpdate FindMuseumSpot =
        pure model
        /\  (Player <<< LocateMuseumSpot <<< Location
                <$> ((/\) <$> Random.random <*> Random.random)
            ) : Nil

    playerUpdate (LocateMuseumSpot location) | not model.museum.open =
        pure model
            { museum
                { host = Blathers
                , facade =
                    case model.museum.facade of
                        Nowhere -> Tent location
                        Tent _ -> Building location
                        Building _ -> Building location
                , open = true
                , speech = Just "WHOOO-HOO!"
                }
            }
        /\ Nil

    playerUpdate (LocateMuseumSpot location) | otherwise =
        (NoLocatingAllowed # Covered.cover model) /\ Nil

update (Host hostAction) model = hostUpdate hostAction where

    hostUpdate (ConsiderSpecies species) | canConsider model.museum =
        pure model
            { museum
                { exposition = newExposition
                , open = isJust $ requiredMore model.museum.facade
                , speech = Just $ speech $ requiredMore model.museum.facade
                }
            }
        /\ (pure $ Player $ GetInReturn leftovers) : Nil
        where
            givenUniqueSpecies = findNewSpecies model.museum.exposition species
            leftovers = findDifference species givenUniqueSpecies
            newExposition = addSpecies model.museum.exposition givenUniqueSpecies
            deliveredAmount = countSpecies givenUniqueSpecies
            newAmount = countSpecies newExposition

            requiredMore Nowhere =
                if upgradeToTentAt - newAmount > 0
                    then Just $ upgradeToTentAt - newAmount
                    else Nothing
            requiredMore (Tent _) =
                if upgradeToBuildingAt - newAmount > 0
                    then Just $ upgradeToBuildingAt - newAmount
                    else Nothing
            requiredMore _ = Nothing

            speech (Just 0) = "Please locate a where to build"
            speech (Just amountNeeded)
                | deliveredAmount <= 1 = progressSpeech amountNeeded
                | deliveredAmount == 1 =
                    "Do you want a story? " <> progressSpeech amountNeeded
                | deliveredAmount > 1 =
                    "Thank you! " <> progressSpeech amountNeeded
                | otherwise = "Weird..."
            speech Nothing | otherwise =
                case model.museum.facade of
                    Building _ -> "Enjoy the day!"
                    Tent _ -> "Please find a location for the building"
                    Nowhere -> "Please find a location for the tent"

            progressSpeech amountNeeded
                | amountNeeded > 0 =
                    case model.museum.facade of
                        Nowhere -> "We need " <> show amountNeeded
                                    <> " more unique species to build a tent."
                        Tent _ -> "We need " <> show amountNeeded
                                    <> " more unique species to aquire a building."
                        _ -> "Everything's Fine.."
                | otherwise = "Museum Closed."

    hostUpdate (ConsiderSpecies _) | otherwise
        = (NoSpeciesDeliveryAllowed # Covered.cover model) /\ Nil

    hostUpdate (ConsiderFossils fossils) | canConsiderFossils model.museum =
        pure model
        /\ (pure $ Player $ GetInReturn
                (noSpecies { skeletons = (\(Fossil sp) -> sp) <$> fossils })
           ) : Nil

    hostUpdate (ConsiderFossils _) | otherwise
        = (NoFossilsDeliveryAllowed # Covered.cover model) /\ Nil

update ProduceError model =
    (ProducedError # Covered.cover model) /\ Nil


update' :: Action -> Covered Error Model -> Covered Error Model /\ List (Effect Action)
update' action covered =
    update action $ Covered.recover covered


view :: Model -> Html Action
view model =
    H.div
        []
        [ H.h3 [] [ H.text "Player" ]
        , button "Dig" $ Player Dig
        , button "Catch" $ Player Catch
        , button "Go Fishing" $ Player GoFishing
        , button' "Deliver" model.museum.open $ Player Deliver
        , button' "Deliver Fossils" model.museum.open $ Player DeliverFossils
        , button' "Find Museum Spot" (not model.museum.open) $ Player FindMuseumSpot
        , button "Produce Error" ProduceError
        , H.h5 [] [ H.text "Species" ]
        , showSpecies model.player
        , H.hr []
        , H.h3 [] [ H.text "Museum" ]
        , H.h5 [] [ H.text $ "Host: " <> show model.museum.host ]
        , H.text $ "Says: " <> case model.museum.speech of
            Just speech -> speech
            Nothing -> "nothing."
        , H.h5 [] [ H.text $ "Open: " <> show model.museum.open ]
        , H.h5 [] [ H.text $ "Facade: " <> show model.museum.facade ]
        , H.h5 [] [ H.text "Exposition" ]
        , showSpecies model.museum.exposition
        ]

    where
        button label action =
            button' label true action
        button' label isEnabled action =
            H.button
                [ H.onClick $ const $ Just $ action
                --, H.enabled isEnabled
                ]
                [ H.text label ]
        showSpecies :: Species -> Html Action
        showSpecies species =
            H.div
                [ H.style
                        $ "display: flex;"
                       <> "width: 40%;"
                       <> "flex-direction: row;"
                       <> "justify-content: space-between;"
                ]
                [ showList "fish" $ loadAmounts species.fish
                , showList "bugs" $ loadAmounts species.bugs
                , showList "skeletons" $ loadAmounts species.skeletons
                , showList "fossils" $ case List.head species.fossils of
                    Just fossil -> pure $ fossil /\ List.length species.fossils
                    Nothing -> Nil
                ]
        showList
            :: forall a
             . Show a
            => String -> List (a /\ Int) -> Html Action
        showList label items =
            H.ul []
                $ H.li []
                    <$> pure <$> H.text
                    <$> (\(item /\ amount) -> show amount <> "x" <> show item)
                    <$> List.toUnfoldable items


view' :: Covered Error Model -> Html Action
view' covered =
    case covered of
        Carried model -> view model
        Recovered error model ->
            H.div
                [ ]
                [ view model
                , H.text $ "Latest errors: " <> show error
                ]


app :: App
app =
    Ui.make' update' view'


loadAmounts :: forall a. Ord a => Eq a => List a -> List (a /\ Int)
loadAmounts values =
    (\vs -> NEList.head vs /\ NEList.length vs)
        <$> List.toUnfoldable (List.group $ List.sort values)


countSpecies :: Species -> Int
countSpecies species =
    List.length species.fish
    + List.length species.bugs
    + List.length species.skeletons


addSpecies :: Species -> Species -> Species
addSpecies museum other =
    { fish : museum.fish <> other.fish
    , bugs : museum.bugs <> other.bugs
    , skeletons : museum.skeletons <> other.skeletons
    , fossils : museum.fossils <> other.fossils
    }


findNewSpecies :: Species -> Species -> Species
findNewSpecies museum player =
    { fish : List.difference (List.nub player.fish) museum.fish
    , bugs : List.difference (List.nub player.bugs) museum.bugs
    , skeletons : List.difference (List.nub player.skeletons) museum.skeletons
    , fossils : List.difference (List.nub player.fossils) museum.fossils
    }


findDifference :: Species -> Species -> Species
findDifference wereGiven beenUseful =
    { fish : List.difference wereGiven.fish beenUseful.fish
    , bugs : List.difference wereGiven.bugs beenUseful.bugs
    , skeletons : List.difference wereGiven.skeletons beenUseful.skeletons
    , fossils : List.difference wereGiven.fossils beenUseful.fossils
    }


addSkeleton species skeletonPart = species { skeletons = skeletonPart : species.skeletons }
addFossil species fossil = species { fossils = fossil : species.fossils }
addBug species bug = species { bugs = bug : species.bugs }
addFish species fish = species { fish = fish : species.fish }


instance semigroupError :: Semigroup Error where
    append (SeveralErrors listA) (SeveralErrors listB) = SeveralErrors $ listA <> listB
    append singleError (SeveralErrors list) = SeveralErrors $ singleError : list
    append (SeveralErrors list) singleError = SeveralErrors $ list <> pure singleError
    append singleErrorA singleErrorB = SeveralErrors $ pure singleErrorA <> pure singleErrorB


instance showError :: Show Error where
    show ProducedError = "Produced Error."
    show (SeveralErrors list) = String.joinWith " " $ List.toUnfoldable $ show <$> list
    show NoLocatingAllowed = "Locating is not allowed now."
    show NoSpeciesDeliveryAllowed = "Delivering species is not allowed now."
    show NoFossilsDeliveryAllowed = "Delivering fossils is not allowed now."


derive instance eqSkeletonPart :: Eq SkeletonPart
derive instance eqFossil :: Eq Fossil
derive instance eqBug :: Eq Bug
derive instance eqFish :: Eq Fish
derive instance eqFacade :: Eq MuseumFacade
derive instance eqLocation :: Eq Location
derive instance ordSkeletonPart :: Ord SkeletonPart
derive instance ordFossil :: Ord Fossil
derive instance ordBug :: Ord Bug
derive instance ordFish :: Ord Fish


derive instance genericSkeletonPart :: Generic SkeletonPart _
derive instance genericFish :: Generic Fish _
derive instance genericBug :: Generic Bug _
derive instance genericFacade :: Generic MuseumFacade _


instance showSkeletonPart :: Show SkeletonPart where show = genericShow
instance showFish :: Show Fish where show = genericShow
instance showBug :: Show Bug where show = genericShow
instance showFossil :: Show Fossil where show _ = "{@}"
instance showHost :: Show Host
    where show TomNook = "Tom Nook"
          show Blathers = "Blathers"
instance showLocation :: Show Location
    where
        show (Location (x /\ y))
            = (show $ floor (x * 100.0)) <> "x" <> (show $ floor (y * 100.0))
instance showFacade :: Show MuseumFacade where show = genericShow
