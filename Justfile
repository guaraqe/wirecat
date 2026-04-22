set shell := ["bash", "-cu"]

build:
    cabal build

test:
    cabal test

format:
    ormolu --mode inplace $(git ls-files '*.hs')

run-tutorial:
    cabal run tutorial-example

viewer-install:
    cd viewer && npm install

viewer-dev:
    cd viewer && npm run dev

viewer-build:
    cd viewer && npm run build
