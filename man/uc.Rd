% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Units.R
\name{uc}
\alias{uc}
\title{Unit conversion function}
\usage{
uc(x, from, to)
}
\arguments{
\item{x}{A quantity to be conversed (atomic vector or list)}

\item{from}{Valid unit name (character)}

\item{to}{Valid unit name (character)}
}
\value{
Converted quantity (atomic vector or list as per input)
}
\description{
Unit conversion function "from"-"to"
}
\examples{
 uc(1, "mkm2", "D")
 uc(1, "mkm2", "mD")
 uc(u(1:5, "kg"), "kg", "lb")
 uc(u(list(1, 2), "ton"), "ton", "lb")
 uc(u(data.frame(x1 = 1:3, x2 = 4:6), "ft3"), "ft3", "cm3")
 uc(1, "ft3", "l")
 uc(1, "ft3", "cm3")
 uc(1, "bbl", "l")
 uc(150, "m", "km") + u(c(1.0, 2.0, 3.5), "km")
 uc(1.5, "cP", "Pa*s")
 # Example below with correct units
 uc(1, "kg/m3", "lb/ft3")
 # Example below with incorrect units causes an error
 \dontrun{uc(1, "kg/m3", "lbft3")}
}
\references{
\enumerate{
  \item \url{http://en.wikipedia.org/wiki/Systems_of_measurement}
  \item \url{http://en.wikipedia.org/wiki/Conversion_of_units}
  \item \url{http://en.wikipedia.org/wiki/Api_gravity}
  \item \url{http://cran.r-project.org/web/packages/marelac/}
  \item \url{http://petrowiki.org}
 }
}
