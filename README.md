[![GitHub release](https://img.shields.io/github/release/jacobwilliams/FGIF.svg)](https://github.com/jacobwilliams/FGIF/releases/latest)
[![Build Status](https://github.com/jacobwilliams/FGIF/actions/workflows/CI.yml/badge.svg)](https://github.com/jacobwilliams/FGIF/actions)
[![codecov](https://codecov.io/gh/jacobwilliams/FGIF/branch/master/graph/badge.svg)](https://codecov.io/gh/jacobwilliams/FGIF)
[![last-commit](https://img.shields.io/github/last-commit/jacobwilliams/FGIF)](https://github.com/jacobwilliams/FGIF/commits/master)

FGIF: Create Animated GIFs with Fortran

## Description

Just a simple module that can be used to create GIFs and animated GIFs with Fortran.
Based on the public domain code at: http://fortranwiki.org/fortran/show/writegif

## Compiling

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

## Documentation

The latest API documentation can be found [here](http://jacobwilliams.github.io/FGIF/). This was generated from the source code using [FORD](https://github.com/Fortran-FOSS-Programmers/ford).

## Examples

### Mandelbrot

![mandelbrot](media/mandelbrot.gif)

### Circle illusion
![animated_gif_1](media/animated_gif_1.gif)

### Game of Life
![game_of_life](media/game_of_life.gif)

### Plasma
![plasma](media/plasma.gif)

### Traveling Salesman Problem
![tsp](media/tsp.gif)

### Maze generation and solving
![maze](media/maze.gif)

### Rotating wireframe cube
![rotating_cube](media/rotating_cube.gif)

### L-system plant growth
![lsystem_plant](media/lsystem_plant.gif)

### N-body gravity simulation
![nbody](media/nbody.gif)

### Falling sand
![falling_sand](media/falling_sand.gif)

### Boids flocking simulation
![boids](media/boids.gif)

### A* pathfinding
![pathfinding](media/pathfinding.gif)

### Rotating wireframe sphere
![rotating_sphere](media/rotating_sphere.gif)
