[![GitHub release](https://img.shields.io/github/release/jacobwilliams/FGIF.svg)](https://github.com/jacobwilliams/FGIF/releases/latest)
[![Build Status](https://github.com/jacobwilliams/FGIF/actions/workflows/CI.yml/badge.svg)](https://github.com/jacobwilliams/FGIF/actions)
[![codecov](https://codecov.io/gh/jacobwilliams/FGIF/branch/master/graph/badge.svg)](https://codecov.io/gh/jacobwilliams/FGIF)
[![last-commit](https://img.shields.io/github/last-commit/jacobwilliams/FGIF)](https://github.com/jacobwilliams/FGIF/commits/master)

FGIF: Create Animated GIFs with Fortran

### Description

Just a simple module that can be used to create GIFs and animated GIFs with Fortran.
Based on the public domain code at: http://fortranwiki.org/fortran/show/writegif

### Compiling

A `fpm.toml` file is provided for compiling `fgif` with the [Fortran Package Manager](https://github.com/fortran-lang/fpm). For example, to build:

```
  fpm build --profile release --flag "-fopenmp"
```

And to run the unit tests:

```
  fpm test --profile release --flag "-fopenmp"
```

To use `fgif` within your fpm project, add the following to your `fpm.toml` file:
```toml
[dependencies]
fgif = { git="https://github.com/jacobwilliams/fgif.git" }
```

### Documentation

The latest API documentation can be found [here](http://jacobwilliams.github.io/FGIF/). This was generated from the source code using [FORD](https://github.com/Fortran-FOSS-Programmers/ford).

### Examples

![mandelbrot](media/mandelbrot.gif)
![animated_gif_1](media/animated_gif_1.gif)
![game_of_life](media/game_of_life.gif)
![plasma](media/plasma.gif)
![tsp](media/tsp.gif)
![maze](media/maze.gif)
![rotating_cube](media/rotating_cube.gif)
![lsystem_plant](media/lsystem_plant.gif)
![nbody](media/nbody.gif)
![falling_sand](media/falling_sand.gif)
![boids](media/boids.gif)
![pathfinding](media/pathfinding.gif)
![rotating_sphere](media/rotating_sphere.gif)
