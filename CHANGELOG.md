# Changelog

## Version 0.3

* Improvement: Drop keypoints which lie close to the image border. Do not
  calculate `mod` when accessing arrays.
* Improvement: Faster gradinent interpolation
* Improvement: Use LAPACK to calculate A^-1*b (a matrix inverse multiplied by a
  vector).

## Version 0.2

* Incompatible change: The system `sift` is renamed to `sift/core` and
  `sift/all` to `sift`. The package `sift` is renamed to `sift/core`.
* Incompatible change: This library works now with single precision floats and
  arrays of single precision floats. This saves 2x memory and gives for about
  15% improvement in speed.
* Improvement: Implement the RANSAC algorithm for image registration.
* Improvement: Document functions in `sift/debug` package.
