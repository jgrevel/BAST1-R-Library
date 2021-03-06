% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_sas7bdat.R
\name{read_sas7bdat}
\alias{read_sas7bdat}
\title{Fast reading of .sas7bdat files.}
\usage{
read_sas7bdat(file, column_information)
}
\arguments{
\item{file}{a character string containing the path of the .sas7bdat file.}

\item{column_information}{a character string or object which will be assigned the column description information.}
}
\value{
A data frame with containing the .sas7bdat data and column descriptions assigned to column_information object.
}
\description{
Fast reading of .sas7bdat files.
}
\examples{
\donttest{
dat = read_TAB(p = "...\\\\data.sas7bdat",column_information = cols)
}
}
\author{
Aaron Hayman
}
