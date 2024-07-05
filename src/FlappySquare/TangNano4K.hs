{-# LANGUAGE NumericUnderscores, RecordWildCards, TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module FlappySquare.TangNano4K where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as E
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.Clock
import Data.Maybe

import RetroClash.VGA640x480
import FlappySquare
import FlappySquare.TMDSSerializer
import FlappySquare.GowinClkDiv
import FlappySquare.GowinPLL
import FlappySquare.GowinLVDS
import FlappySquare.GowinOSerdes



data TMDSOut dom = TMDSOut
    { tmdsClk :: "TMDS_CLK"   ::: LVDS dom
    , tmdsR   :: "TMDS_RED"   ::: LVDS dom
    , tmdsG   :: "TMDS_GREEN" ::: LVDS dom
    , tmdsB   :: "TMDS_BLUE"  ::: LVDS dom
    }

rstSync ::
  Clock Dom25 ->
  Reset Dom25 ->
  Reset Dom25
rstSync clk ext_reset =
  let rstCnt = E.register clk ext_reset enableGen (0 :: BitVector 4)
               (rstCnt + (zeroExtend . pack . complement <$> rstn))
      rstn = reduceAnd <$> rstCnt
   in unsafeFromActiveLow (bitCoerce <$> rstn)
{-# NOINLINE rstSync #-}

topEntity
    :: "CLK_27MHZ" ::: Clock Dom27
    -> "RESETN"    ::: Reset Dom25
    -> "BTN"       ::: Signal Dom25 (Active Low)
    -> ( "HDMI"    ::: TMDSOut Dom125
       , "LED"     ::: Signal Dom25 Bool
       )
topEntity clk rst btn =
  ( TMDSOut (gowinLVDS hdmiClk) (gowinLVDS redS) (gowinLVDS greenS) (gowinLVDS blueS)
  , led
  )
 where
  (VGAOut{..}, led) = flappy clkPix rstS (toActive . fromActive <$> btn)
  VGASync{..} = vgaSync
  clkPix = gowinClkDiv clkSer (unsafeFromActiveLow pllLock)
  (clkSer, pllLock) = gowinPLL clk
  rstS = rstSync clkPix (unsafeFromActiveLow (unsafeToActiveLow rst .&&. pllLock))

  hdmiClk = setName @"serializerClk" gowinOSER10 clkPix clkSer
              (pure (high :> high :> high :> high :> high :>
                     low  :> low :> low :> low :> low :>
                      Nil))

  redS = setName @"redSerializer" synchEncSerializer
            (pack <$> vgaR)
            (pack <$> (bundle (vgaHSync,vgaVSync)))
            vgaDE
            clkPix
            clkSer

  greenS = setName @"greenSerializer" synchEncSerializer
            (pack <$> vgaG)
            (pure 0)
            vgaDE
            clkPix
            clkSer

  blueS = setName @"blueSerializer" synchEncSerializer
            (pack <$> vgaB)
            (pure 0)
            vgaDE
            clkPix
            clkSer

{-# NOINLINE flappy #-}
flappy
    :: "CLK_27MHZ" ::: Clock Dom25
    -> "RESET"     ::: Reset Dom25
    -> "BTN"       ::: Signal Dom25 (Active High)
    -> ("VGA"      ::: VGAOut Dom25
       ,"LED"      ::: Signal Dom25 Bool
       )
flappy clk rst = withEnableGen board clk rst
  where
    board (fmap fromActive -> btn) = (vga, led)
      where
        state = regEn initState newFrame (updateState <$> btn <*> state)
        (vga, newFrame) = video state
        led = regEn False (cnt .==. pure maxBound) (not <$> led)
        cnt = register (0 :: Index 25_200_000)
                   (satAdd SatWrap 1 <$> cnt)


{-# NOINLINE video #-}
video
    :: (HiddenClockResetEnable dom)
    => (DomainPeriod dom ~ HzToPeriod 25_200_000)
    => "STATE" ::: Signal dom St
    -> ("VGA" ::: VGAOut dom
       , "NEW_FRAME" ::: Signal dom Bool
       )
video state = (vgaOut vgaSync rgb, newFrame)
  where
    VGADriver{..} = vgaDriver640x480at60
    newFrame = isFalling False (isJust <$> vgaY)
    rgb = draw <$> state <*> (fromJust <$> vgaX) <*> (fromJust <$> vgaY)

makeTopEntity 'topEntity
