module Test.Main
    ( main ) where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (launchAff_)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Console as Console

import Control.Alt ((<|>))
import Data.List as List
import Data.List ((:), List(..))
import Data.Maybe
import Data.Tuple.Nested ((/\))
import Data.Covered as Covered
import Data.Covered (Covered(..))

import Test.UUID (UUID)
import Test.UUID as UUID

import Test.Spec (Spec, describe, it, pending', pending)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Fsm as Fsm
import Fsm (Fsm)
import CoveredFsm (CoveredFsm)


data Error
  = ErrorOne
  | ErrorTwo


data Model
  = Empty
  | HoldsString String
  | HoldsAction Action
  | HoldsUUID UUID
  | HoldsError Error


emptyModel :: Model
emptyModel = Empty


data Action
  = NoOp
  | ActionOne
  | StoreUUID UUID


spec :: Spec Unit
spec = do

  describe "Fsm" do

    describe "creating" do

      it "is easy to create" do
        let (fsm :: Fsm Action Unit) = Fsm.make (\_ _ -> unit /\ Nil)
        pure unit

      it "is easy to run" do
        let (fsm :: Fsm Action Unit) = Fsm.make (\_ _ -> unit /\ Nil)
        _ <- liftEffect $ Fsm.run fsm (const pure unit) unit
        pure unit

      it "is easy to run with some actions" do
        let fsm = Fsm.make (\_ _ -> unit /\ Nil)
        { push } <- liftEffect $ Fsm.run fsm (const pure unit) unit
        liftEffect $ push NoOp
        pure unit

    describe "updating" do

      it "calls the update function" do
        let
          (myFsm ::Fsm Action Model) = Fsm.make (\_ _ -> HoldsString "foo" /\ Nil)
        lastModel <- liftEffect $ do
          ref <- Ref.new Empty
          { push } <- Fsm.run myFsm (flip Ref.write ref) emptyModel
          push NoOp
          Ref.read ref
        lastModel `shouldEqual` (HoldsString "foo")

      it "the action is actually sent" do
        let
          myFsm = Fsm.make (\action _ -> HoldsAction action /\ Nil)
        lastModel <- liftEffect $ do
          ref <- Ref.new Empty
          { push } <- Fsm.run myFsm (flip Ref.write ref) emptyModel
          push ActionOne
          Ref.read ref
        lastModel `shouldEqual` (HoldsAction ActionOne)

      it "receives all the actions which were sent" do
        let
          myFsm = Fsm.make (\action _ -> HoldsAction action /\ Nil)
        lastModel <- liftEffect $ do
          ref <- Ref.new Empty
          { push } <- Fsm.run myFsm (flip Ref.write ref) emptyModel
          push ActionOne
          push NoOp
          Ref.read ref
        lastModel `shouldEqual` (HoldsAction NoOp)

    describe "effects" do

      it "performs the effect from the update function" do
        let
          updateF NoOp model = model /\ (UUID.new >>= pure <<< StoreUUID) : Nil
          updateF (StoreUUID uuid) _ = HoldsUUID uuid /\ Nil
          updateF _ model = model /\ Nil
          myFsm = Fsm.make updateF
        lastModel <- liftEffect $ do
          ref <- Ref.new Empty
          { push } <- Fsm.run myFsm (flip Ref.write ref) emptyModel
          push NoOp
          Ref.read ref
        case lastModel of
          (HoldsUUID _) -> pure unit
          _ -> fail $ "should contain UUID in the model, but " <> show lastModel

      it "works when asking to perform several actions from effectful part of `update`" do
        let
          updateF ActionOne model =
            model /\ pure NoOp : (UUID.new >>= pure <<< StoreUUID): Nil
          updateF (StoreUUID uuid) _ = HoldsUUID uuid /\ Nil
          updateF _ model = model /\ Nil
          myFsm = Fsm.make updateF
        lastModel <- liftEffect $ do
          ref <- Ref.new Empty
          { push } <- Fsm.run myFsm (flip Ref.write ref) emptyModel
          push ActionOne
          Ref.read ref
        case lastModel of
          (HoldsUUID _) -> pure unit
          _ -> fail $ "should contain UUID in the model, but " <> show lastModel

      it "effectful actions should rule over the model" do
        let
          updateF ActionOne model =
            HoldsString "fail" /\
              (UUID.new >>= pure <<< StoreUUID) : Nil
          updateF (StoreUUID uuid) model =
            HoldsUUID uuid /\ Nil
          updateF _ model =
            model /\ Nil
          myFsm = Fsm.make updateF
        lastModel <- liftEffect $ do
          ref <- Ref.new Empty
          { push } <- Fsm.run myFsm (flip Ref.write ref) emptyModel
          push ActionOne
          Ref.read ref
        case lastModel of
          (HoldsUUID _) -> pure unit
          _ -> fail $ "should contain UUID in the model, but " <> show lastModel

    describe "folding" do

      it "folds the models to the very last state" do
        let
          updateF NoOp _ = HoldsString "NoOp" /\ Nil
          updateF ActionOne _ = HoldsString "ActionOne" /\ Nil
          updateF _ model = model /\ Nil
          myFsm = Fsm.make updateF
        lastModel <- liftEffect
            $ Fsm.fold myFsm emptyModel $ NoOp : ActionOne : List.Nil
        lastModel `shouldEqual` HoldsString "ActionOne"

      it "folds the models to the very last state with effects as well" do
        let
          updateF NoOp model = model /\ (UUID.new >>= pure <<< StoreUUID) : Nil
          updateF (StoreUUID uuid) _ = HoldsUUID uuid /\ Nil
          updateF _ model = model /\ Nil
          myFsm = Fsm.make updateF
        lastModel <- liftEffect
            $ Fsm.fold myFsm emptyModel $ NoOp : ActionOne : List.Nil
        case lastModel of
          (HoldsUUID _) -> pure unit
          _ -> fail $ "should contain UUID in the model, but " <> show lastModel

      it "effectful actions should rule over the model" do
        let
          updateF ActionOne model =
            HoldsString "fail" /\
              (UUID.new >>= pure <<< StoreUUID) : Nil
          updateF (StoreUUID uuid) model =
            HoldsUUID uuid /\ Nil
          updateF _ model =
            model /\ Nil
          myFsm = Fsm.make updateF
        lastModel <- liftEffect
          $ Fsm.fold myFsm emptyModel $ ActionOne : List.Nil
        case lastModel of
          (HoldsUUID _) -> pure unit
          _ -> fail $ "should contain UUID in the model, but " <> show lastModel

    describe "stopping" do

      pending "TODO"

  describe "CoveredFsm" do

    it "passes error through update cycle" do
        let
          (myCovererdFsm :: CoveredFsm Error Action Model) =
              Fsm.make (\_ _ -> Covered.cover Empty ErrorOne /\ Nil)
        lastModel <- liftEffect $ do
          ref <- Ref.new $ Covered.carry Empty
          { push } <- Fsm.run myCovererdFsm (flip Ref.write ref) $ Covered.carry emptyModel
          push NoOp
          Ref.read ref
        case lastModel of
          Recovered ErrorOne _ -> pure unit
          _ -> fail $ "does not contain ErrorOne, but " <> show lastModel

    it "keeps the latest error" do
        let
          updateF NoOp _ = Covered.cover Empty ErrorOne /\ Nil
          updateF ActionOne _ = Covered.cover Empty ErrorTwo /\ Nil
          updateF _ _ = Covered.carry Empty /\ Nil
          (myCovererdFsm :: CoveredFsm Error Action Model)
            = Fsm.make updateF # Fsm.joinWith ((<|>))
        lastModel <- liftEffect $ do
          ref <- Ref.new $ Covered.carry Empty
          { push }  <- Fsm.run myCovererdFsm (flip Ref.write ref) $ Covered.carry emptyModel
          push NoOp
          push ActionOne
          Ref.read ref
        case lastModel of
          Recovered ErrorTwo _ -> pure unit
          _ -> fail $ "does not contain ErrorTwo, but " <> show lastModel

    it "folding also keeps the latest error" do
        let
          updateF NoOp _ = Covered.cover Empty ErrorOne /\ Nil
          updateF ActionOne _ = Covered.cover Empty ErrorTwo /\ Nil
          updateF _ _ = Covered.carry Empty /\ Nil
          (myCovererdFsm :: CoveredFsm Error Action Model)
              = Fsm.make updateF # Fsm.joinWith ((<|>))
        lastModel <- liftEffect
            $ Fsm.fold myCovererdFsm (Covered.carry emptyModel)
                  $ NoOp : ActionOne : List.Nil
        case lastModel of
          Recovered ErrorTwo _ -> pure unit
          _ -> fail $ "does not contain ErrorTwo, but " <> show lastModel

    it "folding keeps the latest error even if there were a success after" do
        let
          updateF NoOp _ = Covered.cover Empty ErrorOne /\ Nil
          updateF ActionOne _ = Covered.carry Empty /\ Nil
          updateF _ _ = Covered.carry Empty /\ Nil
          (myCovererdFsm :: CoveredFsm Error Action Model)
              = Fsm.make updateF # Fsm.joinWith ((<|>))
        lastModel <- liftEffect
            $ Fsm.fold myCovererdFsm (Covered.carry emptyModel)
                  $ NoOp : ActionOne : List.Nil
        case lastModel of
          Recovered ErrorOne _ -> pure unit
          _ -> fail $ "does not contain ErrorOne, but " <> show lastModel

    it "provides the way to collect errors" do
        let
          updateF NoOp c = Covered.cover Empty [ ErrorOne ] /\ Nil
          updateF ActionOne _ = Covered.cover Empty [ ErrorTwo ] /\ Nil
          updateF _ _ = Covered.carry Empty /\ Nil
          (myCovererdFsm :: CoveredFsm (Array Error) Action Model)
            = Fsm.make updateF # Fsm.joinWith Covered.appendErrors
        lastModel <- liftEffect $ do
          ref <- Ref.new $ Covered.carry Empty
          { push } <- Fsm.run myCovererdFsm (flip Ref.write ref) $ Covered.carry emptyModel
          push NoOp
          push ActionOne
          Ref.read ref
        case lastModel of
          Recovered [ ErrorOne, ErrorTwo ] _ -> pure unit
          _ -> fail $ "does not contain [ ErrorOne, ErrorTwo ], but " <> show lastModel


instance showAction :: Show Action where
    show NoOp = "NoOp"
    show ActionOne = "ActionOne"
    show (StoreUUID u) = "Store UUID" <> show u


instance showModel :: Show Model where
    show Empty = "Empty"
    show (HoldsString s) = "HoldsString: " <> s
    show (HoldsAction a) = "HoldsAction: " <> show a
    show (HoldsUUID u) = "HoldsUUID: " <> show u
    show (HoldsError e) = "HoldsError: " <> show e


instance showError :: Show Error where
    show ErrorOne = "ErrorOne"
    show ErrorTwo = "ErrorTwo"


instance eqAction :: Eq Action where
    eq NoOp NoOp = true
    eq ActionOne ActionOne = true
    eq (StoreUUID uuidA) (StoreUUID uuidB) = uuidA == uuidB
    eq _ _ = false


instance eqModel :: Eq Model where
    eq Empty Empty = true
    eq (HoldsString sA) (HoldsString sB) = sA == sB
    eq (HoldsAction aA) (HoldsAction aB) = aA == aB
    eq (HoldsUUID uA) (HoldsUUID uB) = uA == uB
    eq (HoldsError eA) (HoldsError eB) = eA == eB
    eq _ _ = false


derive instance eqError :: Eq Error


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] spec
