% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functins.R
\name{make_filename}
\alias{make_filename}
\title{Year file name creator
\code{make_filename} will create a file name using the inputted year
in the format of \code{accident_year.csv.bz}.}
\usage{
make_filename(year)
}
\arguments{
\item{year}{a number representing the year of the file you want to create
must be a numeric type}
}
\value{
returns a string in the format of \code{accident_year.csv.bz} where
year is a the number of year that was inputted
}
\description{
Year file name creator
\code{make_filename} will create a file name using the inputted year
in the format of \code{accident_year.csv.bz}.
}
\note{
Errors will be thrown if the input year is not a numeric
}
\examples{
\dontrun{
make_filename(2015)
make_filename(2013)
}
}
