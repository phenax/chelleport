default:
  @just --choose

run *args:
  cabal run chelleport -- {{args}}

test *args:
  cabal test {{args}}

testw *args:
  nodemon -e .hs -w src --exec 'ghcid -c "cabal repl test:specs" -T :main'

build:
  nix build
