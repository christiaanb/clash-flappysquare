{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module FlappySquare.TMDSSerializer where

import Clash.Explicit.Prelude hiding (scanr1)
import Clash.Sized.Internal.BitVector (popCountBV)

import Data.Bool (bool)

import FlappySquare.GowinOSerdes

synchEncSerializer ::
  KnownDomain system =>
  Signal system (BitVector 8) -> -- pixel component
  Signal system (BitVector 2) -> -- bus control
  Signal system Bool ->
  Clock system ->
  Clock serialClock ->
  Signal serialClock Bit
synchEncSerializer pixel ctrl de pclk sclk = tmds
 where
  tmdsWord = mux de (Data <$> pixel) (Control <$> ctrl)
  tmdsChar = tmdsEncode pclk tmdsWord
  tmds = setName @"serializer" gowinOSER10 pclk sclk (unpack <$> tmdsChar)
{-# NOINLINE synchEncSerializer #-}

data TMDSWord
  = Data (BitVector 8)
  | Control (BitVector 2)
  deriving (Eq, Show, Generic, NFDataX)

tmdsEncode ::
  KnownDomain dom =>
  Clock dom ->
  Signal dom TMDSWord ->
  Signal dom (BitVector 10)
tmdsEncode clk i = delay clk enableGen 0b1101010100 o
 where
  (sN,o) = unbundle (tmdsEncode1 <$> s <*> i)
  s      = delay clk enableGen 0 sN

tmdsEncode1 :: Signed 4 -> TMDSWord -> (Signed 4, BitVector 10)
tmdsEncode1 acc = \case
  Control c -> (0, ) $ case c of
    0b00 -> 0b1101010100
    0b01 -> 0b0010101011
    0b10 -> 0b0101010100
    _    -> 0b1010101011
  Data d ->
    let pop        = popCountBV d

        (tag1, op) = if pop > 4 || pop == 4 && testBit d 1
          then (False, xnor)
          else (True, xor)

        stage1 :: BitVector 8
        stage1 = bitCoerce (scanr1 op (bitCoerce d))

        stage2 :: BitVector 8
        (tag2, stage2, accN) =
          let pop1 = popCountBV stage1
              popDiff :: Signed 4
              popDiff = 2 * (bitCoerce pop1 - 4)
              (invert, acc')
                | acc == 0 || pop1 == 4
                = (complement tag1, if tag1 then popDiff else negate popDiff)
                | acc > 0 && pop1 > 4 || acc < 0 && pop1 < 4
                = (True, bool 0 2 tag1 - popDiff)
                | otherwise
                = (False, bool (-2) 0 tag1 + popDiff)
          in  (invert, if invert then complement stage1 else stage1, acc')
    in  ( acc + accN
        , bitCoerce (boolToBit tag2 :> boolToBit tag1 :> bitCoerce stage2)
        )


scanr1 :: (a -> a -> a) -> Vec (n + 1) a -> Vec (n + 1) a
scanr1 f (xs :< x) = scanr f x xs

xnor :: Bit -> Bit -> Bit
xnor a b = complement (xor a b)
