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
  loop'

loop' = do

  threadDelay 1000000
  rotateServo 0 0

  threadDelay 1000000
  rotateServo 0 90

  threadDelay 1000000
  rotateServo 0 180

  threadDelay 1000000
  rotateServo 0 90

  threadDelay 1000000
  rotateServo 0 0

  loop'

