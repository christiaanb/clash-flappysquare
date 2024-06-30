{-# LANGUAGE NumericUnderscores, RecordWildCards, TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module FlappySquare.TangNano4K where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.Clock
import Data.Maybe

import RetroClash.VGA720x480
import FlappySquareWide
import FlappySquare.TMDSSerializer
import FlappySquare.GowinPLL
import FlappySquare.GowinLVDS
import FlappySquare.GowinOSerdes



data TMDSOut dom = TMDSOut
    { tmdsClk :: "TMDS_CLK"   ::: LVDS dom
    , tmdsR   :: "TMDS_RED"   ::: LVDS dom
    , tmdsG   :: "TMDS_GREEN" ::: LVDS dom
    , tmdsB   :: "TMDS_BLUE"  ::: LVDS dom
    }

topEntity
    :: "CLK_27MHZ" ::: Clock Dom27
    -> "RESET"     ::: Reset Dom27
    -> "BTN"       ::: Signal Dom27 (Active Low)
    -> "HDMI"      ::: TMDSOut Dom135
topEntity clk rst btn =
  TMDSOut (gowinLVDS hdmiClk) (gowinLVDS redS) (gowinLVDS greenS) (gowinLVDS blueS)
 where
  VGAOut{..} = flappy clk rst (toActive . fromActive <$> btn)
  VGASync{..} = vgaSync
  clkSer = gowinPLL clk

  hdmiClk = setName @"serializerClk" gowinOSER10 clk clkSer
              (pure (high :> high :> high :> high :> high :>
                     low  :> low  :> low  :> low  :> low  :> Nil))

  redS = setName @"redSerializer" synchEncSerializer
            (pack <$> vgaR)
            (pack <$> (bundle (vgaHSync,vgaVSync)))
            vgaDE
            clk
            clkSer

  greenS = setName @"greenSerializer" synchEncSerializer
            (pack <$> vgaG)
            (pure 0)
            vgaDE
            clk
            clkSer

  blueS = setName @"blueSerializer" synchEncSerializer
            (pack <$> vgaB)
            (pure 0)
            vgaDE
            clk
            clkSer

{-# NOINLINE flappy #-}
flappy
    :: "CLK_27MHZ" ::: Clock Dom27
    -> "RESET"     ::: Reset Dom27
    -> "BTN"       ::: Signal Dom27 (Active High)
    -> "VGA"       ::: VGAOut Dom27
flappy clk rst = withEnableGen board clk rst
  where
    board (fmap fromActive -> btn) = vga
      where
        state = regEn initState newFrame (updateState <$> btn <*> state)
        (vga, newFrame) = video state

{-# NOINLINE video #-}
video
    :: (HiddenClockResetEnable dom)
    => (DomainPeriod dom ~ HzToPeriod 27_000_000)
    => "STATE" ::: Signal dom St
    -> ("VGA" ::: VGAOut dom
       , "NEW_FRAME" ::: Signal dom Bool
       )
video state = (vgaOut vgaSync rgb, newFrame)
  where
    VGADriver{..} = vgaDriver720x480at60
    newFrame = isFalling False (isJust <$> vgaY)
    rgb = draw <$> state <*> (fromJust <$> vgaX) <*> (fromJust <$> vgaY)

makeTopEntity 'topEntity
