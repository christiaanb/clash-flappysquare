{-# LANGUAGE RecordWildCards #-}
module FlappySquare where

import Clash.Prelude
import RetroClash.VGA640x480 (Color, ScreenWidth, ScreenHeight, between)

data St = MkSt
    { birdY      :: !(Signed 10)
    , birdSpeed  :: !(Signed 10)
    , pipeOffset :: !(Index ScreenWidth)
    , gameOver   :: !Bool
    , autoPilot  :: !Bool
    }
    deriving (Show, Generic, NFDataX)

initState :: St
initState = MkSt
    { pipeOffset = 0
    , birdSpeed  = 0
    , birdY      = 200
    , gameOver   = False
    , autoPilot  = True
    }

birdX :: Index ScreenWidth
birdX = 100

around :: (Ord a, Num a) => a -> (a, a) -> Bool
x `around` (p, r) = x `between` (p - r, p + r)

pipes :: Vec 10 (Index ScreenHeight, Index ScreenHeight)
pipes =
    (130, 290) :>
    (80,  280) :>
    (60,  270) :>
    (90,  320) :>
    (110, 430) :>
    (200, 410) :>
    (230, 400) :>
    (130, 380) :>
    (90,  330) :>
    (110, 280) :>
    Nil

updateState :: Bool -> Bool -> St -> St
updateState btn btn2 st@MkSt{..}
  | gameOver = initState {autoPilot = autoPilot}
  | otherwise = st
    { pipeOffset = satAdd SatWrap 1 pipeOffset
    , birdSpeed = if btn then birdSpeed - 5 else birdSpeedAuto
    , birdY = birdY + birdSpeed `shiftR` 3
    , gameOver = not birdClear
    , autoPilot = if btn2 then not autoPilot else autoPilot
    }
  where
    (top, bottom, offset) = pipeAt birdX st
    birdClear = fromIntegral birdY `between` (top + 20, bottom - 20)
    birdSafeBottom = fromIntegral birdY <= (bottom - 55)
    birdSpeedAuto = if autoPilot && ((not birdSafeBottom || birdSpeed > 17) && (birdSpeed >= (-38))) then birdSpeed - 5 else birdSpeed + 1

pipeAt :: Index ScreenWidth -> St -> (Index ScreenHeight, Index ScreenHeight, Index 64)
pipeAt x MkSt{..} = (top, bottom, offset)
  where
    idx :: Index 10
    (idx, offset) = bitCoerce (satAdd SatWrap x pipeOffset)
    (top, bottom) = pipes !! idx

draw :: St -> Index ScreenWidth -> Index ScreenHeight -> Color
draw st@MkSt{..} x y
    | isBird = yellow
    | isPipe = pipeColor
    | otherwise = if gameOver then red else blue
  where
    isBird =
        x `around` (birdX, 20) &&
        y `around` (fromIntegral birdY, 20)

    isPipe = not (y `between` (top, bottom))
    (top, bottom, offset) = pipeAt x st
    pipeColor
      | offset < 2  = gray
      | offset < 10 = lightGreen
      | otherwise   = green

blue, yellow, red, gray, green, lightGreen :: Color
blue = (0x80, 0x80, 0xf0)
yellow = (0x00, 0x00, 0x00)
red = (0x80, 0x00, 0x00)
green = (0x92, 0xe2, 0x44)
lightGreen = (0xa5, 0xff, 0x4d)
gray = (0x30, 0x30, 0x30)
