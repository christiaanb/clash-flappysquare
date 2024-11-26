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



data TMDSOut domPix domSer = TMDSOut
    { tmdsClk :: "TMDS_CLK"   ::: LVDS domPix
    , tmdsR   :: "TMDS_RED"   ::: LVDS domSer
    , tmdsG   :: "TMDS_GREEN" ::: LVDS domSer
    , tmdsB   :: "TMDS_BLUE"  ::: LVDS domSer
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
    -> "RESETN"    ::: Signal Dom25 (Active Low)
    -> "BTN"       ::: Signal Dom25 (Active Low)
    -> ( "HDMI"    ::: TMDSOut Dom25 Dom125
       , "LED"     ::: Signal Dom25 Bool
       )
topEntity clk rst btn =
  ( TMDSOut (gowinCLVDS clkPix) (gowinLVDS redS) (gowinLVDS greenS) (gowinLVDS blueS)
  , led
  )
 where
  (VGAOut{..}, led) = flappy clkPix rstS (toActive . fromActive <$> rst) (toActive . fromActive <$> btn)
  VGASync{..} = vgaSync
  clkPix = gowinClkDiv clkSer (unsafeFromActiveLow pllLock)
  (clkSer, pllLock) = gowinPLL clk
  rstS = rstSync clkPix (unsafeFromActiveLow pllLock)

  -- hdmiClk = setName @"serializerClk" gowinOSER10 clkPix clkSer
  --             (pure (high :> high :> high :> high :> high :>
  --                    low  :> low :> low :> low :> low :>
  --                     Nil))

  redS = setName @"redSerializer" synchEncSerializer
            (pack <$> vgaR)
            (pure 0) -- (pack <$> (bundle (vgaHSync,vgaVSync)))
            vgaDE
            clkPix
            clkSer

  greenS = setName @"greenSerializer" synchEncSerializer
            (pack <$> vgaG)
            (pure 0) -- (pack <$> (bundle (vgaHSync,vgaVSync)))
            vgaDE
            clkPix
            clkSer

  blueS = setName @"blueSerializer" synchEncSerializer
            (pack <$> vgaB)
            (pack <$> bundle (vgaVSync,vgaHSync))
            vgaDE
            clkPix
            clkSer

{-# NOINLINE flappy #-}
flappy
    :: "CLK_27MHZ" ::: Clock Dom25
    -> "RESET"     ::: Reset Dom25
    -> "BTN2"      ::: Signal Dom25 (Active High)
    -> "BTN"       ::: Signal Dom25 (Active High)
    -> ("VGA"      ::: VGAOut Dom25
       ,"LED"      ::: Signal Dom25 Bool
       )
flappy clk rst = withEnableGen board clk rst
  where
    board (fmap fromActive -> btn2) (fmap fromActive -> btn) = (vga, led)
      where
        state = regEn initState newFrame (updateState <$> btn <*> btn2 <*> state)
        (vga, newFrame) = video state
        led = regEn False (cnt .==. pure maxBound) (not <$> led)
        cnt = register (0 :: Index (Div 25_200_000 2))
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
