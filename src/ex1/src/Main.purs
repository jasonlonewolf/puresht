module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Math (pi, sqrt)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea r = r * pi

square :: Number -> Number -> Number
square h w = h * w

main :: Effect Unit
main = do
    log "Hello world!1111"
    logShow (diagonal 3.0 4.0)
    logShow (circleArea 3.0)
    logShow (square 3.0 3.0)
