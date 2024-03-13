{-# LANGUAGE NumericUnderscores #-}
module Main where

import GHC.Float
import Control.Concurrent

import System.RaspberryPi.GPIO
import Control.Servo.PWM

main :: IO ()
main = do
  preparePWMChannel 0
  enablePinPWM 0 Pin12
  loop 0

loop :: Int -> IO ()
loop x = do
  threadDelay 100_000
  rotateServo 0 (int2Float x)
  loop (x + 1 `mod` 180)

