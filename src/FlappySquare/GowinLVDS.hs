{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module FlappySquare.GowinLVDS where

import Prelude

import Control.Monad.State (State)
import Data.List.Infinite (Infinite(..), (...))
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import Text.Show.Pretty (ppShow)

import Clash.Annotations.Primitive (Primitive(..), HDL(..), hasBlackBox)
import Clash.Backend (Backend)
import Clash.Netlist.Types (TemplateFunction(..), BlackBoxContext)

import Clash.Sized.BitVector (Bit)
import Clash.Signal.Internal (Signal)
import Clash.NamedTypes ((:::))

import qualified Clash.Netlist.Id as Id
import qualified Clash.Netlist.Types as N
import qualified Clash.Primitives.DSL as DSL
-- import qualified DSL

data LVDS dom = LVDS
  { pos :: "POS" ::: Signal dom Bit
  , neg :: "NEG" ::: Signal dom Bit
  }

gowinLVDS :: Signal dom Bit -> LVDS dom
gowinLVDS s = LVDS s s
{-# OPAQUE gowinLVDS #-}
{-# ANN gowinLVDS hasBlackBox #-}
{-# ANN gowinLVDS
  let
    primName = show 'gowinLVDS
    tfName = show 'gowinLVDSTF
  in InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      format: Haskell
      templateFunction: #{tfName}
  |] #-}

gowinLVDSTF :: TemplateFunction
gowinLVDSTF =
  TemplateFunction
    [inp]
    (const True)
    gowinLVDSTF#
 where
   inp
    :< _ = (0...)

gowinLVDSTF# :: Backend backend => BlackBoxContext -> State backend Doc
gowinLVDSTF# bbCtx
  | [ inp
    ] <- map fst (DSL.tInputs bbCtx)
  , [DSL.ety -> resultTy] <- DSL.tResults bbCtx
  = do

    let
      compName :: Text
      compName = "ELVDS_OBUF"

    instName <- Id.make (compName <> "_inst")
    DSL.declarationReturn bbCtx (compName <> "_block") $ do
      pos_o <- DSL.declare "pos_o" N.Bit
      neg_o <- DSL.declare "neg_o" N.Bit

      let
        generics :: [(Text, DSL.TExpr)]
        generics = [ ]

        inps :: [(Text, DSL.TExpr)]
        inps =
          [ ("I", inp)
          ]

        outs :: [(Text, DSL.TExpr)]
        outs =
          [ ("O", pos_o)
          , ("OB", neg_o)
          ]

      DSL.instDecl
        N.Empty
        (Id.unsafeMake compName)
        instName
        generics
        inps
        outs

      pure [DSL.constructProduct resultTy [pos_o,neg_o]]

gowinPLLTF# bbCtx = error (ppShow bbCtx)
