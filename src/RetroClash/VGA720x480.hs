{-# LANGUAGE ScopedTypeVariables, NumericUnderscores #-}
{-# LANGUAGE DuplicateRecordFields, RecordWildCards, ApplicativeDo #-}
module RetroClash.VGA720x480 where

import Clash.Prelude
import RetroClash.Clock
import Data.Maybe (isJust)
import Data.Word (Word8)

data VGASync dom = VGASync
    { vgaHSync :: "HSYNC" ::: Signal dom Bit
    , vgaVSync :: "VSYNC" ::: Signal dom Bit
    , vgaDE    :: "DE"    ::: Signal dom Bool
    }

data VGAOut dom = VGAOut
    { vgaSync  :: VGASync dom
    , vgaR     :: "RED"   ::: Signal dom Word8
    , vgaG     :: "GREEN" ::: Signal dom Word8
    , vgaB     :: "BLUE"  ::: Signal dom Word8
    }

data VGADriver dom w h = VGADriver
    { vgaSync :: VGASync dom
    , vgaX    :: Signal dom (Maybe (Index w))
    , vgaY    :: Signal dom (Maybe (Index h))
    }

strengthen :: forall n k. (KnownNat n, KnownNat k) => Index (n + k) -> Maybe (Index n)
strengthen x
  | x <= fromIntegral (maxBound @(Index n)) = Just $ fromIntegral x
  | otherwise = Nothing

{-# INLINE between #-}
between :: (Ord a) => a -> (a, a) -> Bool
x `between` (lo, hi) = lo <= x && x <= hi

sync :: Bit -> Bool -> Bit
sync polarity b = if b then polarity else complement polarity

type ScreenWidth = 720
type ScreenHeight = 480

countWhen
    :: (KnownNat n, 1 <= n, HiddenClockResetEnable dom)
    => Signal dom Bool
    -> (Signal dom (Index n), Signal dom Bool)
countWhen inc = (cnt, inc .&&. cnt .==. pure maxBound)
  where
    cnt = regEn 0 inc $ satAdd SatWrap 1 <$> cnt

count
    :: (KnownNat n, 1 <= n, HiddenClockResetEnable dom)
    => (Signal dom (Index n), Signal dom Bool)
count = countWhen (pure True)

{-# NOINLINE vgaDriver720x480at60 #-}
vgaDriver720x480at60
    :: (HiddenClockResetEnable dom)
    => (DomainPeriod dom ~ HzToPeriod 27_000_000)
    => VGADriver dom ScreenWidth ScreenHeight
vgaDriver720x480at60 = VGADriver{ vgaSync = VGASync{..}, .. }
  where
    (hcount, endLine) = count @858
    (vcount, _) = countWhen @525 endLine

    vgaX = strengthen <$> hcount
    vgaY = strengthen <$> vcount

    vgaHSync = sync low . (`between` (720 + 16, 720 + 16 + 62 - 1)) <$> hcount
    vgaVSync = sync low . (`between` (480 + 9, 480 + 9 + 6 - 1)) <$> vcount
    vgaDE = isJust <$> vgaX .&&. isJust <$> vgaY

type Color = (Word8, Word8, Word8)

vgaOut
    :: (HiddenClockResetEnable dom)
    => VGASync dom
    -> Signal dom Color
    -> VGAOut dom
vgaOut vgaSync@VGASync{..} rgb = VGAOut{..}
  where
    (vgaR, vgaG, vgaB) = unbundle $ blank rgb

    blank = mux (not <$> vgaDE) (pure (0, 0, 0))
