# Changelog

## Version 0.5.1

This is a bugfix release.

## Version 0.5

* Improvement: Use CLAHE contrast enhancement algorithm
* Improvement: Use entzauberte-matrices instead of MAGICL.

## Version 0.4

* Incompatible change: `sift/debug` system and package were renamed to
  `sift/util`.
* Improvement: A new function `enhance-contrast` which enhances contrast of an
  image for better keypoint detection. This function implements the simpliest
  histogram equalization algorithm.
* Improvement: `affine-transform` returns sum of the least squares as the second
  value.
* Improvement: Nearest neighbors approximation is used for computation of
  orientation and descriptors. This is much faster as bilinear approximation,
  but produces the same number of correctly matched keypoints.

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
