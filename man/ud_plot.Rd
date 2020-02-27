\name{ud_plot}
\alias{ud_plot}

\title{
Draws an UpAndDown plot to display percentage and absolute changes
}
\description{
Function for drawing UpAndDown plots for up to 3 levels
}
\usage{
ud_plot(outPrep, b=0, totperc="yes", vscale=NULL, labelvar=NULL,
        levelColour="none", barColour=levelColour, ud_control=ud_colours())
}
\arguments{
  \item{outPrep}{
a list of the grouping variables chosen, the sorting methods for them, nesting information, the numbers of categories in them, and the sorted dataset to be plotted
}
  \item{b}{
the baseline value from which the bars are drawn.  The default is 0.  It can be informative to set it to the overall rate or to some threshold value.
}
  \item{totperc}{
whether the dashed line for overall percentage change is to be drawn.  The default is "yes".  It is useful to set it to "no" if you draw an UpAndDown plot for a subset and do not want to draw the rate for the subset in this way.
}
  \item{vscale}{
sets the limits for the vertical axis scale.  Default limits are calculated from the data.
}
  \item{labelvar}{
the name of the variable to be used for labelling.  If specified, it has to be one of the first two grouping variables.
}
  \item{levelColour}{
 specifies by variable name which level will be coloured (even if there is only one level).  The default is not to colour them, other than grey for the top level bars.
}
  \item{barColour}{
specifies a character or factor variable to colour (fill) the bars.  The default is to use the levelColour variable itself. 
}
  \item{ud_control}{
a list of colours for UpAndDown plots. If omitted, \code{\link{ud_colours}} gives the defaults.
}
}
\details{
This function takes the output from \code{\link{ud_prep}} and draws an UpAndDown plot showing percentage and absolute changes for up to three levels.  The levels may be nested or not.  There are a number of options for sorting, labelling, and colouring the displays.
The plots produced are ggplot2 objects, so you can amend them---to some extent---yourself.
Choosing a barColour variable with many different colours is generally not a good idea and takes a while to draw because of the lengthy legend.  See {\code{\link{ud_colours}}} for choosing palettes.
}

\value{
  \item{uad}{
an unlabelled vertical UpAndDown plot
}
  \item{uadl}{
a horizontal UpAndDown plot with labelling of groups or items (NULL if no labelvar is set)
}
  \item{TotPerc}{
the percentage change for all the data used in the plot (the value displayed by the red dashed line)
}
  \item{level1}{
the category names and percentage changes plotted for the first level specified in the input levs
}
  \item{level2}{
the category names and percentage changes plotted for the second level specified in the input levs (NULL if there is only one level)
}
  \item{level3}{
the category names and percentage changes plotted for the third level specified in the input levs (NULL if there are not three levels)
}
}

\author{
Antony Unwin unwin@math.uni-augsburg.de
}

\seealso{\code{\link{ud_colours}}}

\examples{
yy <- ud_prep(CPIuk, weight="Weight", v1="Aug2017", v2="Aug2018",
levs=c("Sector", "Subsector", "Component"), sortLev=c("perc", "perc", "perc"))
y1 <- ud_plot(yy, b=0, vscale=NULL, labelvar="Sector")
y1$uad
y1$uadl
}

