\name{ud_prep}
\alias{ud_prep}

\title{
Prepares data for drawing an UpAndDown plot
}
\description{
Checks input parameters and sorts the data
}
\usage{
ud_prep(data, weight=1, v1, v2, levs, sortLev)
}
\arguments{
  \item{data}{
the dataset to be analysed
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
  \item{levs}{
up to three grouping variables to define the plot levels in order from highest to lowest
}
  \item{sortLev}{
how the grouping variables named in levs are to be sorted.  Each entry must be one of "orig", "base", "perc", "abs" (i.e., no sorting, sorting by initial value, sorting by percentage change, sorting by absolute change).
}
}
\details{
This function draws UpAndDown plots showing percentage and absolute changes for up to three levels.  There are a number of options for sorting the data.  If a level is nested, then it is sorted within the next highest level.  If a level is not nested, then it is sorted as if it were the top level.
}

\value{
  \item{levs}{
the variables specifying the grouping levels for the plot
}
  \item{sortLev}{
the sorting method used for each plot level
}
  \item{hx}{
a summary of the nesting of the grouping variables
}
  \item{lgv}{
the numbers of categories in the grouping variables
}
  \item{data}{
an extended version of the dataset comprising the original variables and the derived absolute and percentage changes for the different levels specified in levs.  The levels are sorted according to the sortLev settings chosen and the corresponding percentages.
}
}

\author{
Antony Unwin unwin@math.uni-augsburg.de
}

\examples{
yy <- ud_prep(CPIuk, weight="Weight", v1="Aug2017", v2="Aug2018",
levs=c("Sector", "Subsector", "Component"), sortLev=c("perc", "perc", "perc"))
}

