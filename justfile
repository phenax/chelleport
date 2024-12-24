default:
  @just --choose

run *args:
  cabal run chelleport -- {{args}}

runw *args:
  nodemon -e .hs,.cpp -w bin -w src -w cpp --exec 'clear && just run {{args}}'

test *args:
  cabal test {{args}}

testw *args:
  nodemon -e .hs -w src -w specs --exec 'clear && just test {{args}}'

build:
  nix build

appimage:
  nix bundle --bundler github:ralismark/nix-appimage

# lib:
#   @mkdir -p dist-lib;
#   gcc -o dist-lib/libchelleport.so \
#     cpp/libchelleport.cpp \
#     -shared \
#     -lstdc++ \
#     $(pkg-config --libs tesseract lept);
