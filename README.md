UpAndDownPlots
================
Antony Unwin
February 27, 2020

[![CRAN
Status](http://www.r-pkg.org/badges/version/UpAndDownPlots)](https://cran.r-project.org/package=UpAndDownPlots)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/UpAndDownPlots)](http://www.r-pkg.org/pkg/UpAndDownPlots)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Travis-CI Build
Status](https://travis-ci.org/heike/UpAndDown.svg?branch=master)](https://travis-ci.org/heike/UpAndDown)
[![Last-changedate](https://img.shields.io/badge/last%20change-2020--02--27-yellowgreen.svg)](/commits/master)

# UpAndDownPlots

UpAndDown plots display percentage changes by height and absolute
changes by area for up to three hierarchical levels. They can visualise
changes in indices, showing how the changes for sectors or for
individual components contribute to the overall change.

## Installation

The package `UpAndDownPlots` is available from github at the moment:

``` r
devtools::install_github("heike/UpAndDown")
```

## Example

The Northern Ireland population grew by 3.12% between 2011 and 2017.
What were the changes by the four age groups reported?

``` r
library(UpAndDownPlots)
popx <- ud_prep(NIpop, v1="y2011", v2="y2017", levs=c("age"), sortLev=c("orig"))

p1 <- ud_plot(popx, labelvar="age")
p1$uadl
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

The 65+ age group increased by almost 20% and the 16-39 group actually
declined. It is useful to note that the older group were less than half
of the size of the 16-39 group in 2011.

There are more examples in the package vignettes.
