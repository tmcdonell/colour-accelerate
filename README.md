<div align="center">
<img width="450" src="https://github.com/AccelerateHS/accelerate/raw/master/images/accelerate-logo-text-v.png?raw=true" alt="henlo, my name is Theia"/>

# colour-accelerate

[![GitHub CI](https://github.com/tmcdonell/colour-accelerate/workflows/CI/badge.svg)](https://github.com/tmcdonell/colour-accelerate/actions)
[![Gitter](https://img.shields.io/gitter/room/nwjs/nw.js.svg)](https://gitter.im/AccelerateHS/Lobby)
<br>
[![Stackage LTS](https://stackage.org/package/colour-accelerate/badge/lts)](https://stackage.org/lts/package/colour-accelerate)
[![Stackage Nightly](https://stackage.org/package/colour-accelerate/badge/nightly)](https://stackage.org/nightly/package/colour-accelerate)
[![Hackage](https://img.shields.io/hackage/v/colour-accelerate.svg)](https://hackage.haskell.org/package/colour-accelerate)

</div>

This package provides data types and operations for dealing with colours in
Accelerate. For details on Accelerate, refer to the [main
repository][accelerate].

Contributions and bug reports are welcome!<br>
Please feel free to contact me through [GitHub][accelerate] or [gitter.im][gitter.im].


Example: Blur
-------------

| Test image | sRGB | RGB |
|:----------:|:----:|:---:|
| ![Test image][blocks] | ![sRGB linear][blur_srgb] | ![RGB non-linear][blur_rgb] |

The test image on the left is composed of blocks and bars of the primary and
secondary colours, arranged so that each colour is juxtaposed next to all
others, plus black and white.

The image in the centre is created by blurring the original with a 9x9 Gaussian
filter in the linear gamma sRGB space.

The image on the right is created by blurring with the same 9x9 Gaussian filter
in the standard non-linear RGB colour space. Note the dark regions separating
red from green and cyan, and blue from red and green; purple lines separate cyan
from red and magenta; green separates yellow from cyan. These regions are
artefacts produced by mixing colours in the RGB colour space.

The files to generate the test image and run the demonstration are in the
examples directory.


[blocks]:     https://github.com/tmcdonell/colour-accelerate/raw/master/samples/blocks.bmp
[blur_srgb]:  https://github.com/tmcdonell/colour-accelerate/raw/master/samples/blur_srgb.bmp
[blur_rgb]:   https://github.com/tmcdonell/colour-accelerate/raw/master/samples/blur_rgb.bmp
[accelerate]: https://github.com/AccelerateHS/accelerate
[gitter.im]:  https://gitter.im/AccelerateHS/Lobby

