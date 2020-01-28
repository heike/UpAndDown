\name{AutoSalesXGermany}
\alias{AutoSalesX}
\docType{data}
\title{
Sales of cars and vans in Germany by manufacturer (edited version of AutoSales).
}
\description{
Sales of cars and vans in Germany for 2017 and 2018.  The market is divided into 14 segments and in the two years 2017 and 2018 there were 50 competing manufacturers.
}
\usage{data(AutoSalesX)}
\format{
  A data frame with 173 observations on the following 5 variables.
  \describe{
    \item{\code{Sector}}{Car, Van, Other}
    \item{\code{Segment}}{Market segment}
    \item{\code{Manufacturer}}{Manufacturer}
    \item{\code{sales17}}{Sales in 2017}
    \item{\code{sales18}}{Sales in 2018}
   }
}
\details{
This is a 'cleaned' and reduced version of the dataset AutoSales from this package.\cr

Vehicles selling less than 1000 in both years have been reclassified as 'Other'.  The data have been aggregated by manufacturer within segments.

}
\source{
Kraftfahrt-Bundesamt \cr
\url{https://www.kba.de/DE/Statistik/Fahrzeuge/Neuzulassungen/MonatlicheNeuzulassungen/monatl_neuzulassungen_node.html}
}
\examples{
library(dplyr)
yx <- ud_prep(AutoSalesX \%>\% filter(Segment=="Compact"), v1="sales17",
      v2="sales18", levs=c("Manufacturer"), sortLev=c("base"))
y5 <- ud_plot(yx, labelvar="Manufacturer")
y5$uadl
}
\keyword{datasets}
