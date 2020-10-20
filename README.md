# JuicyPixels-dither-sa

> Dither [JuicyPixels](https://github.com/Twinside/Juicy.Pixels) images with a silly algorithm.

https://en.wikipedia.org/wiki/Dither

[JuicyPixels](https://github.com/Twinside/Juicy.Pixels) is a Haskell library
to load &amp; save pictures, but it doesn't provide any image manipulation
functionality.

---

## Example images

![Original](https://raw.githubusercontent.com/phadej/JuicyPixels-dither-sa/master/demo/MichelangeloDavid.png)
![Floyd-Steinberg](https://raw.githubusercontent.com/phadej/JuicyPixels-dither-sa/master/demo/MichelangeloDavid-FloydSteinberg.png)
![This package](https://raw.githubusercontent.com/phadej/JuicyPixels-dither-sa/master/demo/MichelangeloDavid-dither.png)

*Original - Floyd-Steinberg - This package*

### Grays

![gray55-or](https://raw.githubusercontent.com/phadej/JuicyPixels-dither-sa/master/demo/gray55.png)
![gray55-fs](https://raw.githubusercontent.com/phadej/JuicyPixels-dither-sa/master/demo/gray55-fs.png)
![gray55-sa](https://raw.githubusercontent.com/phadej/JuicyPixels-dither-sa/master/demo/gray55-sa.png)

![gray66-or](https://raw.githubusercontent.com/phadej/JuicyPixels-dither-sa/master/demo/gray66.png)
![gray66-fs](https://raw.githubusercontent.com/phadej/JuicyPixels-dither-sa/master/demo/gray66-fs.png)
![gray66-sa](https://raw.githubusercontent.com/phadej/JuicyPixels-dither-sa/master/demo/gray66-sa.png)

![gray77-or](https://raw.githubusercontent.com/phadej/JuicyPixels-dither-sa/master/demo/gray77.png)
![gray77-fs](https://raw.githubusercontent.com/phadej/JuicyPixels-dither-sa/master/demo/gray77-fs.png)
![gray77-sa](https://raw.githubusercontent.com/phadej/JuicyPixels-dither-sa/master/demo/gray77-sa.png)

![gray88-or](https://raw.githubusercontent.com/phadej/JuicyPixels-dither-sa/master/demo/gray88.png)
![gray88-fs](https://raw.githubusercontent.com/phadej/JuicyPixels-dither-sa/master/demo/gray88-fs.png)
![gray88-sa](https://raw.githubusercontent.com/phadej/JuicyPixels-dither-sa/master/demo/gray88-sa.png)

![gray99-or](https://raw.githubusercontent.com/phadej/JuicyPixels-dither-sa/master/demo/gray99.png)
![gray99-fs](https://raw.githubusercontent.com/phadej/JuicyPixels-dither-sa/master/demo/gray99-fs.png)
![gray99-sa](https://raw.githubusercontent.com/phadej/JuicyPixels-dither-sa/master/demo/gray99-sa.png)

![grayAA-or](https://raw.githubusercontent.com/phadej/JuicyPixels-dither-sa/master/demo/grayAA.png)
![grayAA-fs](https://raw.githubusercontent.com/phadej/JuicyPixels-dither-sa/master/demo/grayAA-fs.png)
![grayAA-sa](https://raw.githubusercontent.com/phadej/JuicyPixels-dither-sa/master/demo/grayAA-sa.png)

---

- *Does SA stand for simulated annealing or silly algorithm*?
- *Both*
