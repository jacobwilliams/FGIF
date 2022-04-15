![fgif](media/mandelbrot.gif)
============

[![GitHub release](https://img.shields.io/github/release/jacobwilliams/fgif.svg)](https://github.com/jacobwilliams/fgif/releases/latest)
[![Build Status](https://github.com/jacobwilliams/fgif/actions/workflows/CI.yml/badge.svg)](https://github.com/jacobwilliams/fgif/actions)
[![codecov](https://codecov.io/gh/jacobwilliams/fgif/branch/master/graph/badge.svg?token=43HK33CSMY)](https://codecov.io/gh/jacobwilliams/fgif)
[![last-commit](https://img.shields.io/github/last-commit/jacobwilliams/fgif)](https://github.com/jacobwilliams/fgif/commits/master)

FGIF: Create Animated GIFs with Fortran

### Description

Just a simple module that can be used to create GIFs and animated GIFs with Fortran.
Based on the public domain code at: http://fortranwiki.org/fortran/show/writegif

### Compiling

A `fmp.toml` file is provided for compiling `fgif` with the [Fortran Package Manager](https://github.com/fortran-lang/fpm). For example, to build:

```
  fpm build --profile release
```

And to run the unit tests:

```
  fpm test --profile release
```

To use `fgif` within your fpm project, add the following to your `fpm.toml` file:
```toml
[dependencies]
fgif = { git="https://github.com/jacobwilliams/fgif.git" }
```
