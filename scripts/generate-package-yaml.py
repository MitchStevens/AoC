#!/usr/bin/env python3

import yaml

DEPENDENCIES = [
  "base >= 4.19 && < 5",
  "array",
  "comonad",
  "containers",
  "ghc",
  "indexed-traversable",
  "lens",
  "megaparsec",
  "mtl",
  "parser-combinators",
  "unordered-containers",
  "semialign",
  "split",
  "shelly",
  "transformers",
  "text", 
  "vector",
  "hashable",
  "witherable",
]

OTHER_DEPENDENCIES = {
  "2023": {
    "1": [ "sbv" ]
  }
}
# "algebraic-graphs",
# "sbv",
# "these",
#  "group",
#  "hashable",
#  "matrix",


DEFAULT_EXTENSIONS = [
  "FlexibleContexts",
  "DeriveFunctor",
  "DeriveFoldable",
  "DeriveTraversable",
  "DeriveGeneric",
  "TypeFamilies",
  "FlexibleInstances",
  "MultiWayIf",
  "LambdaCase",
  "MultiParamTypeClasses",
  "ApplicativeDo",
  "BlockArguments",
  "OverloadedStrings",
  "TemplateHaskell",
  "TupleSections",
  "ScopedTypeVariables",
  "TypeOperators",
]


GHC_OPTIONS  = []
# "ghc-options": [
#     #"-Wall",
#     #"-Wcompat",
#     #"-Widentities",
#     #"-Wincomplete-record-updates",
#     #"-Wincomplete-uni-patterns",
#     #"-Wmissing-export-lists",
#     #"-Wmissing-home-modules",
#     #"-Wpartial-fields",
#     #"-Wredundant-constraints",
#   ],


def advent(year: int, day: int) -> dict:
  other_deps = OTHER_DEPENDENCIES.get
  return {
    "main": "Main",
    "source-dirs": [ "./src/", f"./app/{year}/Day{day}/" ],
    "ghc-options": [],
    "dependencies": DEPENDENCIES + OTHER_DEPENDENCIES.get(year, {}).get(day, []),
  }


import os
import re

executables = { 
  "start-advent": {
    "main": "StartAdvent.Main",
    "source-dirs": [ "./app/Advent/" ],
    "dependencies": [ "optparse-applicative" ],
  },
  "run-advent": {
    "main": "RunAdvent.Main",
    "source-dirs": [ "./app/Advent/" ],
    "dependencies": [ "optparse-applicative" ],
  }
}

for year in range(2023, 2024 + 1) :
  for day in range(1, 25 + 1):
    if (os.path.exists(f"app/{year}/Day{day}/Main.hs")):
      executables[f"advent-{year}-{day}"] = advent(year, day)

package = {
  "name": "advent",
  "author": "Mitch Stevens",
  "dependencies": DEPENDENCIES,
  "ghc-options": GHC_OPTIONS,
  "default-extensions": DEFAULT_EXTENSIONS,
  "executables": executables
}


with open('package.yaml', 'w') as outfile:
    yaml.dump(package, outfile, default_flow_style=False)