# CheeseGrater

Cheesegrater is a (very early stage) experimental rasteriser, written in Haskell.

## Objectives

The intent of the project is to follow two main phases:
  1. Implementation of a rasteriser that works on a polygonal representation of geometry, and
     uses an algorithm that clips each polygon against each rendered pixel. This is equivalent
     to box-filtering the geometry.
  2. Implementation of a rasteriser based upon Manson (2013), which involves analytic
     rasterisation of geometry using filter functions expressed as polynomials.

Phase 2 requires geometry that can be clipped to pixel boundaries, so it seems natural to
implement the box filtering as a separate initial phase.

Manson, J. and Schaefer, S. (2013) Analytic Rasterization of Curves with Polynomial FIlters.
Eurographics. 32(2).

## Primitives

The SVG specification seems to be a good indicator of the geometry and fill options that should
be supported. Initially, all geometric primitives will be diced into a polygonal form. Fills
will be represented as `Point -> Color` functions that are presumed to be low-frequency enough
to be sampled only once per pixel.

Later in the project, non-polygonal forms will be investigated for splines.
