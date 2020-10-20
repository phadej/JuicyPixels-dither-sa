#!/bin/sh

cabal build juicydither
JUICYDITHER=$(cabal-plan list-bin juicydither)

run() {
    echo "$@"
    time "$@"
}

# Braille
run "$JUICYDITHER" --iterations 8 demo/MichelangeloDavid.png

# Small image
run "$JUICYDITHER" --iterations 8 demo/phadej-small.png demo/phadej-small-dither.png

# Michelangelo
run "$JUICYDITHER" --iterations 8 demo/MichelangeloDavid.png demo/MichelangeloDavid-dither.png

# Bigger image
run "$JUICYDITHER" --iterations 1 demo/phadej.png demo/phadej-1.png
run "$JUICYDITHER" --iterations 8 demo/phadej.png demo/phadej-dither.png
run "$JUICYDITHER" --algorithm FS demo/phadej.png demo/phadej-fs.png
run "$JUICYDITHER" --iterations 2 --algorithm FSSA demo/phadej.png demo/phadej-fssa-2.png

# Greys
gray() {
	GRAY=$1
	run "$JUICYDITHER" --iterations 8 "demo/gray$GRAY.png" "demo/gray$GRAY-sa.png"
	run "$JUICYDITHER" --algorithm FS "demo/gray$GRAY.png" "demo/gray$GRAY-fs.png"
}

gray 55
gray 66
gray 77
gray 88
gray 99
gray AA
