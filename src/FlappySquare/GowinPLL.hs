{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module FlappySquare.GowinPLL where

import Prelude

import Control.Arrow (second)
import Control.Monad.State (State)
import Data.List.Infinite (Infinite(..), (...))
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import Text.Show.Pretty (ppShow)

import Clash.Annotations.Primitive (Primitive(..), HDL(..), hasBlackBox)
import Clash.Backend (Backend)
import Clash.Netlist.Types (TemplateFunction(..), BlackBoxContext)

import Clash.Signal.Internal (Clock (..), Signal, ResetPolarity (ActiveLow), clockGen, vName, vPeriod, vResetPolarity, hzToPeriod, vSystem, createDomain)

import qualified Clash.Netlist.Id as Id
import qualified Clash.Netlist.Types as N
import qualified Clash.Primitives.DSL as DSL

import Clash.Annotations.TH
-- import qualified DSL

-- | 25 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom27", vPeriod = hzToPeriod 27_000_000}
createDomain vSystem{vName="Dom125", vPeriod = hzToPeriod 126_000_000}
createDomain vSystem{vName="Dom25", vPeriod = hzToPeriod 25_200_000, vResetPolarity=ActiveLow}

gowinPLL :: Clock Dom27 -> (Clock Dom125, Signal Dom25 Bool)
gowinPLL (Clock {}) = (clockGen, pure True)
{-# OPAQUE gowinPLL #-}
{-# ANN gowinPLL hasBlackBox #-}
{-# ANN gowinPLL
  let
    primName = show 'gowinPLL
    tfName = show 'gowinPLLTF
  in InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      format: Haskell
      templateFunction: #{tfName}
  |] #-}

gowinPLLTF :: TemplateFunction
gowinPLLTF =
  TemplateFunction
    [clkSrc]
    (const True)
    gowinPLLTF#
 where
   clkSrc
    :< _ = (0...)

gowinPLLTF# :: Backend backend => BlackBoxContext -> State backend Doc
gowinPLLTF# bbCtx
  | [ clkSrc
    ] <- map fst (DSL.tInputs bbCtx)
  , [DSL.ety -> resultTy] <- DSL.tResults bbCtx
  = do

    let
      compName :: Text
      compName = "PLLVR"

    instName <- Id.make (compName <> "_inst")
    DSL.declarationReturn bbCtx (compName <> "_block") $ do
      lock_o <- DSL.declare "lock_o" N.Bit
      clkout_o <- DSL.declare "clkout_o" N.Bit
      clkoutp_o <- DSL.declare "clkoutp_o" N.Bit
      clkoutd_o <- DSL.declare "clkoutd_o" N.Bit
      clkoutd3_o <- DSL.declare "clkoutd3_o" N.Bit
      gw_vcc <- DSL.assign "gw_vcc" DSL.High
      gw_gnd <- DSL.assign "gw_gnd" DSL.Low

      let
        generics :: [(Text, DSL.TExpr)]
        generics = map (second DSL.litTExpr)
          [ ("FCLKIN", "27")
          , ("DYN_IDIV_SEL", "false")
          , ("IDIV_SEL", 2)
          , ("DYN_FBDIV_SEL", "false")
          , ("FBDIV_SEL", 13)
          , ("DYN_ODIV_SEL", "false")
          , ("ODIV_SEL", 8)
          , ("PSDA_SEL", "0000")
          , ("DYN_DA_EN", "true")
          , ("DUTYDA_SEL", "1000")
          , ("CLKOUT_FT_DIR", 1)
          , ("CLKOUTP_FT_DIR", 1)
          , ("CLKOUT_DLY_STEP", 0)
          , ("CLKOUTP_DLY_STEP", 0)
          , ("CLKFB_SEL","internal")
          , ("CLKOUT_BYPASS","false")
          , ("CLKOUTP_BYPASS","false")
          , ("CLKOUTD_BYPASS","false")
          , ("DYN_SDIV_SEL",2)
          , ("CLKOUTD_SRC","CLKOUT")
          , ("CLKOUTD3_SRC","CLKOUT")
          , ("DEVICE","GW1NSR-4C")
          ]

        inps :: [(Text, DSL.TExpr)]
        inps =
          [ ("CLKIN", clkSrc)
          , ("RESET", gw_gnd)
          , ("RESET_P", gw_gnd)
          , ("CLKFB", gw_gnd)
          , ("FBDSEL", DSL.tuple [gw_gnd,gw_gnd,gw_gnd,gw_gnd,gw_gnd,gw_gnd])
          , ("IDSEL", DSL.tuple [gw_gnd,gw_gnd,gw_gnd,gw_gnd,gw_gnd,gw_gnd])
          , ("ODSEL", DSL.tuple [gw_gnd,gw_gnd,gw_gnd,gw_gnd,gw_gnd,gw_gnd])
          , ("PSDA", DSL.tuple [gw_gnd,gw_gnd,gw_gnd,gw_gnd])
          , ("DUTYDA", DSL.tuple [gw_gnd,gw_gnd,gw_gnd,gw_gnd])
          , ("FDLY", DSL.tuple [gw_gnd,gw_gnd,gw_gnd,gw_gnd])
          , ("VREN", gw_vcc)
          ]

        outs :: [(Text, DSL.TExpr)]
        outs =
          [ ("CLKOUT", clkout_o)
          , ("LOCK", lock_o)
          , ("CLKOUTP", clkoutp_o)
          , ("CLKOUTD", clkoutd_o)
          , ("CLKOUTD3", clkoutd3_o)
          ]

      DSL.instDecl
        N.Empty
        (Id.unsafeMake compName)
        instName
        generics
        inps
        outs

      pure [DSL.constructProduct resultTy [clkout_o,lock_o]]

gowinPLLTF# bbCtx = error (ppShow bbCtx)
