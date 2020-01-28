\name{sort5}
\alias{sort5}

\title{
Draws four UpAndDown plots for one grouping variable with the five standard sorting methods
}
\description{
This function draws plots to compare the results of different sorting methods for a single grouping variable
}
\usage{
sort5(data, gvar, weight=1, v1, v2)
}
\arguments{
  \item{data}{
the dataset to be analysed
}
 \item{gvar}{
the grouping variable to be used in all five plots
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
This function prepares UpAndDown plots showing percentage and absolute changes for one grouping variable with five different sortings.

To draw an unlabelled or labelled set of plots, apply grid.arrange to the appropriate output of sort5.
}

\value{
  \item{uad}{
five unlabelled vertical UpAndDown plots
}
  \item{uadl}{
five horizontal UpAndDown plots with labelling of groups
}

}

\author{
Antony Unwin unwin@math.uni-augsburg.de
}

\examples{
library(gridExtra)
xs <- sort5(NIpop, gvar="LGD2014_name", v1="y2011", v2="y2017")
grid.arrange(xs$uad)
grid.arrange(xs$uadl)
}

