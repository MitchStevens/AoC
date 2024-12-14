#!/usr/bin/env python3

import yaml

package = {
  "name":                "advent-haskell",
  "version":             "0.1.0.0",
  "github":              "githubuser/advent-haskell",
  "license":             "BSD3",
  "author":              "Author name here",
  "maintainer":          "example@example.com",
  "copyright":           "2022 Author name here",

  "extra-source-files":  [ "README.md", "CHANGELOG.md" ],

  # "synopsis":            None,
  # "category":            None,

  "description":         "Please see the README on GitHub at <https://github.com/githubuser/advent-haskell#readme>",

  "dependencies": [
    "base >= 4.7 && < 5",
    "array",
    "comonad",
    "containers",
    "data-fix",
    "free",
    "algebraic-graphs",
    "heap",
    "indexed-traversable",
    "lens",
    "megaparsec",
    "monad-memo",
    "mtl",
    "multiset",
    "parser-combinators",
    "recursion-schemes",
    "semialign",
    "split",
    "matrix",
    "these",
    "transformers",
    "sbv",
    "vector",
    "witherable",
  ],

  "default-extensions": [
    "FlexibleContexts",
    "DeriveFunctor",
    "DeriveFoldable",
    "DeriveTraversable",
    "TypeFamilies",
    "FlexibleInstances",
    "MultiWayIf",
    "LambdaCase",
    "MultiParamTypeClasses",
    "TemplateHaskell",
    "TupleSections",
    "ScopedTypeVariables",
    "TypeOperators",
  ],

  "ghc-options": [
    "-Wall",
    "-Wcompat",
    "-Widentities",
    "-Wincomplete-record-updates",
    "-Wincomplete-uni-patterns",
    "-Wmissing-export-lists",
    "-Wmissing-home-modules",
    "-Wpartial-fields",
    "-Wredundant-constraints",
  ],

  "executables": {}
}

import os
import re

for year in (os.scandir("./src")):
  if not year.name.isdigit():
    break

  for dayFilePath in os.listdir(year):
    day = re.match("Day([0-9]+)", dayFilePath).group(1)
    package["executables"]["advent-" + year.name + "-" + day] = {
      "main": "Main",
      "source-dirs": [ "src/common", f"src/{year.name}/Day{day}" ],
      "ghc-options": [
        "-threaded",
        "-rtsopts",
        "-with-rtsopts=-N",
      ]
    }

with open('package.yaml', 'w') as outfile:
    yaml.dump(package, outfile, default_flow_style=False)

#executables:
#  advent: 
#    main: Main.hs
#    source-dirs:
#    - app
#    - src/2020
#    - src/2021
#    - src/2022
#    - src/2023
#    - src/common
#    ghc-options:
#    - -threaded
#    - -rtsopts
#    - -with-rtsopts=-N
#
#
#tests:
#  # 2022-test:
#  #   main:                Spec.hs
#  #   source-dirs:         
#  #   - test/2022
#  #   - src/2022
#  #   - src/common
#  #   ghc-options:
#  #   - -threaded
#  #   - -rtsopts
#  #   - -with-rtsopts=-N
#  #   dependencies:
#  #   - hspec
#
#  2021-test:
#    main:                Spec.hs
#    source-dirs:         
#    - test/2021
#    - src/2021
#    - src/common
#    ghc-options:
#    - -threaded
#    - -rtsopts
#    - -with-rtsopts=-N
#    dependencies:
#    - hspec
#
#  2020-test:
#    main:                Spec.hs
#    source-dirs:         
#    - test/2020
#    - src/2020
#    - src/common
#    ghc-options:
#    - -threaded
#    - -rtsopts
#    - -with-rtsopts=-N
#    dependencies:
#    - hspec
#
#  common-test:
#    main:                Spec.hs
#    source-dirs:         
#    - test/common
#    - src/common
#    ghc-options:
#    - -threaded
#    - -rtsopts
#    - -with-rtsopts=-N
#    dependencies:
#    - hspec