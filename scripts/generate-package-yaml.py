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
    "base >= 4.19 && < 5",
    "algebraic-graphs",
    "array",
    "comonad",
    "containers",
    "data-fix",
    "optparse-applicative",
    "free",
    "ghc",
    "graphs",
    "indexed-traversable",
    "lens",
    "matrix",
    "hashable",
    "megaparsec",
    "mtl",
    "multiset",
    "parser-combinators",
    "recursion-schemes",
    "pqueue",
    "unordered-containers",
    "monad-memo",
    "sbv",
    "semialign",
    "split",
    "these",
    "transformers",
    "vector",
    "witherable",
  ],

  "library": {
    "dependencies": [],
    "source-dirs": "./app"
  },


  "default-extensions": [
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

for year in (os.scandir("./app")):
  if not year.name.isdigit():
    break

  for dayFilePath in os.listdir(year):
    day = re.match("Day([0-9]+)", dayFilePath).group(1)
    package["executables"]["advent-" + year.name + "-" + day] = {
      "main": f"Main",
      "source-dirs": [ "src/", f"app/{year.name}/Day{day}/" ],
      "ghc-options": [
      ]
    }

package["executables"]["start-advent"] = {
  "main": "StartAdvent.hs",
  "source-dirs": ["src/Advent"]
}



with open('package.yaml', 'w') as outfile:
    yaml.dump(package, outfile, default_flow_style=False)
