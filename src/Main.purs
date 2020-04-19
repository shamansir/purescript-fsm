module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.Covered


main :: Effect Unit
main = do
  log "🍝 Foobar"
  log $ show $ (carry "foo" :: Covered String String)
