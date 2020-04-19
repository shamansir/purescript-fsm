{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "control"
  , "effect"
  , "either"
  , "event"
  , "exceptions"
  , "foldable-traversable"
  , "halogen-vdom"
  , "lists"
  , "maybe"
  , "newtype"
  , "psci-support"
  , "refs"
  , "spork"
  , "tuples"
  , "web-dom"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
