{-# LANGUAGE CPP, NumericUnderscores #-}
{-|

Control a 9g Tower Pro Micro Servo with the RPi hardware PWM.
This library is overfitted for raspberry pi (uses bcm2835 lib underneath).



Example:

@
main = withGPIO do
  preparePWMChannel 0
  enablePinPWM 0 Pin12
  rotateTo 180 Pin12
@


-}
module Control.Servo.PWM where

import Data.Word
import GHC.Float
import System.RaspberryPi.GPIO

-- Clock divider is set to 16.
-- With a divider of 16 and a RANGE of 24000, in MARKSPACE mode,
-- the pulse repetition frequency will be
-- 1.2MHz/24000 = 50Hz, suitable for driving the 9g micro servo motor with PWM
--
--
-- Now we need the data to set, in the 50Hz frame, 1ms to 2ms the output to HIGH
--
-- (we use 1.2MHz because that is (RPi PWM clock frequency/(CLOCK_DIVIDER=16)))

#define RPI_PWM_CLOCK 19_200_000
#define DEF_CLOCK_DIVIDER 16
#define DEF_RANGE 24000


-- | A PWM channel, in the raspberry it can be either 0 or 1
type PWMCh = Word8

-- | This sets the default channel mode (enabled + markspace) and range (24000) for the
-- channel, and clock divider (16), which applies to both channels.
preparePWMChannel :: PWMCh -> IO ()
preparePWMChannel channel = do
  setClockPWM DEF_CLOCK_DIVIDER
  setModePWM channel 1 1
  setRangePWM channel DEF_RANGE
  setDataPWM channel 0

-- | Set the given Pin's function to use the given hardware PWM channel.
enablePinPWM
  :: PWMCh
  -> Pin -- ^ Pin to enable PWM for
  -> IO ()
enablePinPWM 0 Pin12 = setPinFunction Pin12 Alt5
enablePinPWM channel pin = error "todo"


-- | Set a Servo's orientation to the given angle
rotateServo
  :: PWMCh
  -> Float -- ^ Angle to set Servo to, must be in [0, 180]
  -> IO ()
rotateServo channel angle = do
  let
    -- this should be 50Hz, suitable for driving 9g micro servo
    pulse_rep_freq = (RPI_PWM_CLOCK / DEF_CLOCK_DIVIDER) / DEF_RANGE
    -- this should be in the 1ms to 2ms range, i.e. 0.05*pulse_rep_freq to 0.1*pulse_rep_freq, which can be set with 0.05*DEF_RANGE to 0.1*DEF_RANGE
    data_val = (angle / 180) * (0.05 * DEF_RANGE) + (0.05 * DEF_RANGE)
  setDataPWM channel (fromIntegral $ float2Int data_val)

