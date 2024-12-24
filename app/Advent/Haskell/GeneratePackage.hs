{-# LANGUAGE NoFieldSelectors #-}

module Haskell.GeneratePackage where

import Text.Printf
import Data.Map (Map)
import qualified Data.Map as M
import Types

data Executable = Executable
  { ghcOptions :: [String]
  , mainIs :: String
  , sourceDirs :: [String]
  }

executable :: AdventDate -> Executable
executable (AdventDate year day) = Executable
  { ghcOptions =
    [ "-Wall"
    , "-Wcompat"
    , "-Widentities"
    , "-Wincomplete-record-updates"
    , "-Wincomplete-uni-patterns"
    , "-Wmissing-export-lists"
    , "-Wmissing-home-modules"
    , "-Wpartial-fields"
    , "-Wredundant-constraints"
    ]
  , mainIs = "Main"
  , sourceDirs = [ "./app/Advent", printf "./src/%d/Day%d" year day ]
  }

data PackageYaml = PackageYaml
  { defaultExtensions :: [String]
  , dependencies :: [String]
  , executables :: Map String Executable
  }

defaultYaml :: PackageYaml
defaultYaml = PackageYaml
  { dependencies = 
    [ "base >= 4.19 && < 5"
    , "algebraic-graphs"
    , "array"
    , "comonad"
    , "containers"
    , "data-fix"
    , "optparse-applicative"
    , "free"
    , "ghc"
    , "graphs"
    , "indexed-traversable"
    , "lens"
    , "matrix"
    , "hashable"
    , "megaparsec"
    , "mtl"
    , "multiset"
    , "parser-combinators"
    , "recursion-schemes"
    , "unordered-containers"
    , "monad-memo"
    , "sbv"
    , "semialign"
    , "split"
    , "these"
    , "transformers"
    , "vector"
    , "witherable"
    ]
  , defaultExtensions =
    [ "FlexibleContexts"
    , "DeriveFunctor"
    , "DeriveFoldable"
    , "DeriveTraversable"
    , "DeriveGeneric"
    , "TypeFamilies"
    , "FlexibleInstances"
    , "MultiWayIf"
    , "LambdaCase"
    , "MultiParamTypeClasses"
    , "TemplateHaskell"
    , "TupleSections"
    , "ScopedTypeVariables"
    , "TypeOperators"
    ]
  , executables = M.empty
  }

mkPackageYaml :: AdventDate -> PackageYaml
mkPackageYaml date = defaultYaml { executables = M.singleton "advent" (executable date) }

main :: IO ()
main = print 1