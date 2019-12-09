\name{ConsumerPriceIndexUK}
\alias{CPIuk}
\docType{data}
\title{
Consumer Price Index data for the UK for August 2017 and August 2018.
}
\description{
The UK Office for National Statistics publishes updated Consumer Price Index data every month.  There are 12 sectors with from 1 to 7 subsectors and the subsectors comprise up to 9 items.
}
\usage{data(CPIuk)}
\format{
  A data frame with 85 observations on the following 10 variables.
  \describe{
    \item{\code{ID}}{ID number made up of Sector, Subsector, Item numbers}
    \item{\code{SectorID}}{Sector number}
    \item{\code{SubsectorID}}{Subsector number within sector}
    \item{\code{Item}}{Item number within Sector}
    \item{\code{Sector}}{Sector label}
    \item{\code{Subsector}}{Subsector label}
    \item{\code{Component}}{Item label}
    \item{\code{Weight}}{Item weight in 2017}
    \item{\code{Aug2017}}{Item index value in August 2017}
    \item{\code{Aug2018}}{Item index value in August 2018}
  }
}
\source{
Office for National Statistics (ONS) UK \cr
\url{https://www.ons.gov.uk/economy/inflationandpriceindices/datasets/consumerpriceinflation/}
}
\examples{
yw <- ud_prep(CPIuk, weight="Weight", v1="Aug2017", v2="Aug2018",
              levs=c("Sector"), sortLev=c("perc"))
y2 <- ud_plot(yw, labelvar="Sector")
y2$uadl
}
\keyword{datasets}
