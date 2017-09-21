colour-accelerate
=================

[![Build Status](https://travis-ci.org/tmcdonell/colour-accelerate.svg?branch=master)](https://travis-ci.org/tmcdonell/colour-accelerate)
[![Hackage](https://img.shields.io/hackage/v/colour-accelerate.svg)](https://hackage.haskell.org/package/colour-accelerate)

This package provides data types and operations for dealing with colours in
Accelerate. For details on Accelerate, refer to the [main
repository](https://github.com/AccelerateHS/accelerate).


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
