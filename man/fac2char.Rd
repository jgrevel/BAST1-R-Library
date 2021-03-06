% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fac2char.R
\name{fac2char}
\alias{fac2char}
\title{Convert any vectors of factors in a data.frame or list to character vectors.}
\usage{
fac2char(df)
}
\arguments{
\item{df}{data.frame (or list) containing factors}
}
\value{
Returns a data.frame or list (depending on what was supplied) where all factors have been converted to characters.
}
\description{
Converts the factors in lists and data.frames to character. In lists this operates only at the top level (not whole trees in nested lists).
This is generally done because characters are easier to work with.
}
\examples{
d=data.frame(a=1:5,b=letters[5:1])
str(d)
str(fac2char(d))

}
\author{
Aaron Hayman
}
