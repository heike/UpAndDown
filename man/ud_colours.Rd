\name{ud_colours}
\alias{ud_colours}

\title{
Set colours for UpAndDown plots
}
\description{
Provides a colour scheme for UpAndDown plots.
}
\usage{
ud_colours(colours = c("grey75", "grey65", "red", "blue", "green"), colors,
           gcpal=colorblind_pal()(8))
}
\arguments{
  \item{colours}{
A set of colours for UpAndDownplots.

The first one is used for filling bars and the second for the borders of bars;
the third one is for colouring the dashed line for the overall percentage change;
the fourth one is for colouring the borders of rectangles representing the highest grouping level;
the last one is for colouring the borders of a second grouping level (if there is one).
}
  \item{colors}{
To allow users to write 'colors' instead of 'colours'.
}
  \item{gcpal}{
A colour palette to use if bars are to be coloured by a grouping variable (the parameter groupColour in ud_plot).
The default is the colorblind palette from ggthemes, which has 8 colours.  If more are needed, a palette is generated from the randomcoloR package, always using the same seed for replication purposes.
The same procedure applies for a user-defined palette---if there are not enough colours, a randomcoloR palette is used.
}
}
\details{
ud_colours is provided for assigning colours for UpAndDown plots.}
\value{
A named list of colours.
}

\author{
Antony Unwin unwin@math.uni-augsburg.de
}

\seealso{\code{\link{ud_plot}}}

\examples{
# Using default colours
yw <- ud_prep(CPIuk, weight="Weight", v1="Aug2017", v2="Aug2018",
              levs=c("Sector"), sortLev=c("perc"))
y2 <- ud_plot(yw, labelvar="Sector")
y2$uadl

# Using individually chosen colours
library(ggthemes)
y2a <- ud_plot(yw, ud_control=ud_colours(colours = c("lightblue1", "grey50",
               "red4", "blue", "green")))
y2a$uad

# Using groupColour and the default palette
# The randomcoloR palette is used as there are more than 8 sectors.
y2b <- ud_plot(yw, groupColour="Sector")
y2b$uad

# Using groupColour and a specified palette
y2c <- ud_plot(yw, groupColour="Sector", ud_control=ud_colours(
       gcpal=randomcoloR::distinctColorPalette(k=20)))
y2c$uad
}
