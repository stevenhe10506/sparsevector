
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sparsevector

<!-- badges: start -->

[![R-CMD-check](https://github.com/stevenhe10506/sparsevector/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stevenhe10506/sparsevector/actions/workflows/R-CMD-check.yaml)
[![check-standard](https://github.com/stevenhe10506/sparsevector/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/stevenhe10506/sparsevector/actions/workflows/check-standard.yaml)
<!-- badges: end -->

`sparsevector` provides an efficient S4 implementation of sparse numeric
vectors in R.  
It stores only non-zero values and their positions, making it
memory-efficient for high-dimensional vectors.

## Installation

You can install the development version of sparsevector like so:

install.packages(“devtools”)
devtools::install_github(“stevenhe10506/sparsevector”)

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(sparsevector)
#> 
#> Attaching package: 'sparsevector'
#> The following object is masked from 'package:base':
#> 
#>     norm

x <- c(0, 3, 0, 0, 5, 0)
sx <- as(x, "sparse_numeric")
sx
#> Length: 6 
#> with elements 3 5 at positions 2 5
y <- c(1, 0, 0, 2, 0, 0)
sy <- as(y, "sparse_numeric")

sx + sy
#> Length: 6 
#> with elements 1 3 2 5 at positions 1 2 4 5
sx * sy
#> Length: 6 
#> with elements  at positions
```
