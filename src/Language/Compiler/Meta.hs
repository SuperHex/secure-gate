{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RoleAnnotations #-}

module Language.Compiler.Meta (T (..)) where

import Language.Haskell.TH
import Language.Core
import qualified Data.Function as F (fix)

type role T phantom phantom
newtype T st a = T { unT :: Q Exp }

instance Core T where
  int n = T [| n |]
  bool b = T [| b |]
  a .+ b = T [| $(unT a) + $(unT b) |]
  a .- b = T [| $(unT a) - $(unT b) |]
  a .* b = T [| $(unT a) * $(unT b) |]
  lam f  = T $ lamE [varP (mkName "x")] (unT $ f (T $ varE (mkName "x")))
  app f a = T $ appE (unT f) (unT a)
  fix f = T [| F.fix $(unT (lam f)) |]
  if_ p a b = T [| if $(unT p) then $(unT a) else $(unT b) |]
  a .<= b = T [| $(unT a) <= $(unT b) |]
