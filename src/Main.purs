module Main where

import Prelude

import Effect (Effect)

import VDom (embed) as VDom

import Example (app, init) as Example


main :: Effect Unit
main = do
  VDom.embed "#app" Example.app Example.init
