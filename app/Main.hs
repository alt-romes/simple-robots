{-# LANGUAGE NumericUnderscores #-}
module Main where

import GHC.Float
import Control.Concurrent

import System.RaspberryPi.GPIO
import Control.Servo.PWM

main :: IO ()
main = withGPIO $ do -- very important!
  preparePWMChannel 0
  enablePinPWM 0 Pin12
  loop 0

loop :: Int -> IO ()
loop x = do
  threadDelay 100_000
  rotateServo 0 (int2Float x)
  if x + 1 > 180
     then loop 0
     else loop (x + 1)

