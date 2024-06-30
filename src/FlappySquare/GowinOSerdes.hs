{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS -fconstraint-solver-iterations=20 #-}

module FlappySquare.GowinOSerdes where

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

import Clash.Class.BitPack (unpack)
import Clash.Signal.Internal (Signal (..), Clock (..))
import Clash.Sized.BitVector (Bit)
import Clash.Sized.Vector (Vec((:>), Nil))

import qualified Clash.Netlist.Id as Id
import qualified Clash.Netlist.Types as N
import qualified Clash.Primitives.DSL as DSL
-- import qualified DSL

gowinOSER10 :: Clock par -> Clock ser -> Signal par (Vec 10 Bit) -> Signal ser2 Bit
gowinOSER10 Clock{} Clock{} = go
 where
  go :: Signal par (Vec 10 Bit) -> Signal ser Bit
  go ((i0 :> i1 :> i2 :> i3 :> i4 :> i5 :> i6 :> i7 :> i8 :> i9 :> Nil) :- is) =
    i0 :- i1 :- i2 :- i3 :- i4 :- i5 :- i6 :- i7 :- i8 :- i9 :- go is
{-# OPAQUE gowinOSER10 #-}
{-# ANN gowinOSER10 hasBlackBox #-}
{-# ANN gowinOSER10
  let
    primName = show 'gowinOSER10
    tfName = show 'gowinOSER10TF
  in InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      format: Haskell
      templateFunction: #{tfName}
  |] #-}

gowinOSER10TF :: TemplateFunction
gowinOSER10TF =
  TemplateFunction
    [clkPar,clkSer,d]
    (const True)
    gowinOSER10TF#
 where
   clkPar :<
    clkSer :<
    d :< _ = (0...)

gowinOSER10TF# :: Backend backend => BlackBoxContext -> State backend Doc
gowinOSER10TF# bbCtx
  | [ clkPar
    , clkSer
    , d
    ] <- map fst (DSL.tInputs bbCtx)
  , [q] <- DSL.tResults bbCtx
  = do

    let
      compName :: Text
      compName = "OSER10"

      oserName :: Maybe Text -> Text
      oserName Nothing = "OSER10_inst"
      oserName (Just "result") = oserName Nothing
      oserName (Just "__VOID_TDECL_NOOP__") = oserName Nothing
      oserName (Just s) = s

    instName <- Id.make (oserName (N.bbCtxName bbCtx))
    DSL.declaration (compName <> "_block") $ do
      ~[d9,d8,d7,d6,d5,d4,d3,d2,d1,d0] <- DSL.unvec "d" d

      let
        generics :: [(Text, DSL.TExpr)]
        generics = map (second DSL.litTExpr)
          [ ("GSREN", "false")
          , ("LSREN", "false")
          ]

        inps :: [(Text, DSL.TExpr)]
        inps =
          [ ("D0", d0)
          , ("D1", d1)
          , ("D2", d2)
          , ("D3", d3)
          , ("D4", d4)
          , ("D5", d5)
          , ("D6", d6)
          , ("D7", d7)
          , ("D8", d8)
          , ("D9", d9)
          , ("PCLK", clkPar)
          , ("FCLK", clkSer)
          , ("RESET", DSL.Low)
          ]

        outs :: [(Text, DSL.TExpr)]
        outs =
          [ ("Q", q)
          ]

      DSL.instDecl
        N.Empty
        (Id.unsafeMake compName)
        instName
        generics
        inps
        outs

gowinOSER10TF# bbCtx = error (ppShow bbCtx)
