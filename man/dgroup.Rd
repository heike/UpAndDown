\name{dgroup}
\alias{dgroup}

\title{
Draws UpAndDown plots for different grouping variables
}
\description{
This function draws plots to compare the effects of different grouping variables.
}
\usage{
dgroup(data, byvars, weight=1, v1, v2)
}
\arguments{
  \item{data}{
the dataset to be analysed
}
 \item{byvars}{
the grouping variables to be used
}
  \item{weight}{
a variable for weighting the individual items, the default is for every item to be weighted equally
}
  \item{v1}{
the variable specifying the start value in the comparison
}
  \item{v2}{
the variable specifying the end value in the comparison
}
}
\details{
This function prepares UpAndDown plots showing percentage and absolute changes for different grouping variables.  All plots have the same vertical scale to ease comparisons.

To draw an unlabelled or labelled set of plots, apply grid.arrange to the appropriate output of dgroup.
}

\value{
  \item{uad}{
unlabelled vertical UpAndDown plots
}
  \item{uadl}{
horizontal UpAndDown plots with labelling
}

}

\author{
Antony Unwin unwin@math.uni-augsburg.de
}

\examples{
library(tidyverse)
library(gridExtra)
zs <- dgroup(NIpop, byvars=c("age", "gender", "LGD2014_name", "area_name"), v1="y2011", v2="y2017")
grid.arrange(zs$uadg)
grid.arrange(zs$uadgl)
}

