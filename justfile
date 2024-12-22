default:
  @just --choose

run *args:
  cabal run chelleport -- {{args}}

test *args:
  cabal test {{args}}

testw *args:
  # nodemon -e .hs -w src --exec 'ghcid -c "cabal repl test:specs" -T :main'
  nodemon -e .hs -w src -w specs --exec 'clear && just test {{args}}'

build:
  nix build

appimage:
  nix bundle --bundler github:ralismark/nix-appimage
