{-# LANGUAGE CPP #-}
module Passman.Compat (Natural) where

#if MIN_VERSION_base(4,8,0)
import Numeric.Natural (Natural)
#else
type Natural = Integer
#endif
