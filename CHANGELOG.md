# Revision history for raytracer

## 0.1.0.0 -- 2022-08-10
The project as of Stream 1, 10th August 2022.
* Vector maths added.
* Program currently draws a blue 800x600 rectangle in PPM format.


## 0.2.0.0 -- 2022-08-14
The project as of Stream 2, 14th August 2022.
* Hittable class added, along with Spheres.
* Program now draws two spheres, coloured according to their normals.

## 0.3.0.0 -- 2022-08-22
The project as of Stream 3, 22nd August 2022.
* Multisampling antialiasing now added.
    * Default rendering resolution lowered to 400x300 for speed.
* Material class added, with a Diffuse material option.
* Rays now bounce recursively off objects, and attenuate their colour depending on the object's material.

## 0.3.1.0 -- 2022-08-25 minor fixes
Some fixes to the Stream 3 code, and maintenance to the project files.
* Cabal updated to 3.0.
* MIT licence added.
* Compiler flag `-Wall` added, and warnings cleared up around that.
* Added comments to most things in `Main.hs`.
* `Raylude.randomUnit` now uses spherical coordinates to construct a vector.
* The gamma function is now given a gamma value as an argument.
    * The current ray tracing code now uses a gamma value of 2.
* Bugfix: Sphere scattering now returns the next state of the RNG, rather than the before-state.