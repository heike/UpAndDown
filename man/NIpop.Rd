\name{NIpop}
\alias{NIpop}
\docType{data}
\title{
Northern Ireland population estimates 2011-2017
}
\description{
The Northern Ireland Statistics & Research Agency (NISRA) publishes population estimates on their webpage for 80 District Electoral Areas (DEA).  Each of these DEAs is in one of the 11 Local Government Districts (LGD).  The data are further broken down by gender and age groups.  This dataset includes estimates for the years 2011 to 2017.
}
\usage{data(NIpop)}
\format{
  A data frame with 640 observations on the following 11 variables.
  \describe{
    \item{\code{area_name}}{District Electoral Area}
    \item{\code{LGD2014_name}}{Local Government District}
    \item{\code{gender}}{Males or Females}
    \item{\code{age}}{four age groups: 00-15, 16-39, 40-64, 65+}
    \item{\code{y2011}}{population estimates for the year 2011}
    \item{\code{y2012}}{population estimates for the year 2012}
    \item{\code{y2013}}{population estimates for the year 2013}
    \item{\code{y2014}}{population estimates for the year 2014}
    \item{\code{y2015}}{population estimates for the year 2015}
    \item{\code{y2016}}{population estimates for the year 2016}
    \item{\code{y2017}}{population estimates for the year 2017}
  }
}
\source{
Northern Ireland Statistics & Research Agency \cr
\url{https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/MYE17_DEA_1117.xlsx}
}
\examples{
km <- ud_prep(NIpop, v1="y2011", v2="y2017", levs=c("age", "gender"), sortLev=c("orig", "perc"))
k2 <- ud_plot(km, labelvar="age")
k2$uadl
}
\keyword{datasets}
