module VDom
    (embed) where

import Prelude

import Effect (Effect)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Data.Covered (carry)
import Data.Tuple.Nested (type (/\))

import Effect.Exception (throwException, error)
import Effect.Ref as Ref
import Effect.Uncurried as EFn

import FRP.Event as E

import Halogen.VDom as V
import Halogen.VDom.DOM.Prop as P
import Halogen.VDom.Machine as Machine
import Halogen.VDom.Thunk (buildThunk)

import Web.DOM.Element (toNode) as DOMElement
import Web.DOM.Node (appendChild) as DOM
import Web.DOM.ParentNode (querySelector) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toDocument, toParentNode) as HTMLDocument
import Web.HTML.Window (document) as DOM

import Spork.Html (Html)

import Fsm (run) as Fsm
import Ui (view, run) as Ui

import Example (HtmlRenderer, Model)


embed
    :: String -- selector
    -> HtmlRenderer
    -> Model
    -> Effect Unit
embed sel renderer firstModel = do
    doc ← DOM.window >>= DOM.document
    mbEl ← DOM.querySelector (wrap sel) (HTMLDocument.toParentNode doc)
    case mbEl of
        Nothing -> throwException (error ("Element does not exist: " <> sel))
        Just el -> do
            { next, push } <- Ui.run renderer firstModel
            let
                vdomSpec = V.VDomSpec
                    { document : HTMLDocument.toDocument doc
                    , buildWidget: buildThunk unwrap
                    , buildAttributes: P.buildProp push
                    }
            first_vdom ← EFn.runEffectFn1
                            (V.buildVDom vdomSpec)
                            (unwrap $ Ui.view renderer $ carry firstModel)
            vdom_ref <- Ref.new first_vdom -- use recursion istead of `Ref`?
            void $ DOM.appendChild (Machine.extract first_vdom) (DOMElement.toNode el)
            cancel <- E.subscribe next $
                \next_view -> do
                    prev_vdom <- Ref.read vdom_ref
                    next_vdom ← EFn.runEffectFn2 Machine.step prev_vdom (unwrap next_view)
                    _ <- Ref.write next_vdom vdom_ref
                    pure unit
            pure unit
