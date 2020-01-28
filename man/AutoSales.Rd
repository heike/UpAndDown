\name{AutoSalesGermany}
\alias{AutoSales}
\docType{data}
\title{
Sales of cars and vans in Germany in the years 2017 and 2018.
}
\description{
Sales of cars and vans in Germany for 2017 and 2018.  The market is divided into 14 segments and in the two years 2017 and 2018 there were 50 competing manufacturers.
}
\usage{data(AutoSales)}
\format{
  A data frame with 393 observations on the following 7 variables.
  \describe{
    \item{\code{Sector}}{Car, Van, Other}
    \item{\code{Segment}}{The KBA splits the market into 14 segments}
    \item{\code{ModelSeries}}{Full vehicle name reported by KBA}
    \item{\code{Manufacturer}}{Manufacturer}
    \item{\code{Model}}{Model name}
    \item{\code{sales17}}{Sales in 2017}
    \item{\code{sales18}}{Sales in 2018}
   }
}
\details{
The Kraftfahrt-Bundesamt (KBA, National Vehicle Authority in Germany) publishes spreadsheets of sales every month including year-to-date figures.  The full vehicle name has been split into manufacturer and model names.  NAs have been transformed to 0's.  The variable names and market segments have been anglicised.\cr

A 'cleaned' and reduced version of the dataset is available as AutoSalesX in this package.
}
\source{
Kraftfahrt-Bundesamt \cr
\url{https://www.kba.de/DE/Statistik/Fahrzeuge/Neuzulassungen/MonatlicheNeuzulassungen/monatl_neuzulassungen_node.html}
}
\examples{
yy <- ud_prep(AutoSales, v1="sales17", v2="sales18", levs=c("Segment"), sortLev=c("perc"))
y1 <- ud_plot(yy, labelvar="Segment")
y1$uadl
}
\keyword{datasets}
