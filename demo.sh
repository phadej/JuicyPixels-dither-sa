#!/bin/sh

cabal build juicydither
JUICYDITHER=$(cabal-plan list-bin juicydither)

run() {
    echo "$@"
    time "$@"
}

# Braille
run "$JUICYDITHER" --iterations 8 demo/MichelangeloDavid.png

exit 0

# Small image
run "$JUICYDITHER" --iterations 8 demo/phadej-small.png demo/phadej-small-dither.png

# Michelangelo
run "$JUICYDITHER" --iterations 8 demo/MichelangeloDavid.png demo/MichelangeloDavid-dither.png

# Bigger image
run "$JUICYDITHER" --iterations 1 demo/phadej.png demo/phadej-1.png
run "$JUICYDITHER" --iterations 8 demo/phadej.png demo/phadej-dither.png
