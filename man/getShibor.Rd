% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_data.R
\name{getShibor}
\alias{getShibor}
\title{Function to get shibor rate}
\usage{
getShibor(from = "2006-01-01")
}
\arguments{
\item{from}{the starting date of shibor rates}
}
\description{
each record of shibor rates contains one day, one week, two week, one month, three months, six months, nine months and 1 year shibor rates
}
\examples{
init()
getShibor()
}
