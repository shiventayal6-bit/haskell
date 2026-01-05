{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_perfect_trees (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "perfect_trees"
version :: Version
version = Version [1,0,0,0] []

synopsis :: String
synopsis = "Perfect k-ary trees with list utilities"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
