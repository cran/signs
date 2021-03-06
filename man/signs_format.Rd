% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/signs.R
\name{signs_format}
\alias{signs_format}
\title{A function factory to add proper minus signs}
\usage{
signs_format(
  ...,
  format = getOption("signs.format", scales::number),
  add_plusses = getOption("signs.add.plusses", FALSE),
  trim_leading_zeros = getOption("signs.trim.leading.zeros", FALSE),
  label_at_zero = getOption("signs.label.at.zero", "none")
)
}
\arguments{
\item{...}{Other arguments passed on to \code{format}.}

\item{format}{Any function that takes a numeric vector
and returns a character vector,
such as \code{scales::number},
\code{scales::comma}, or
\code{scales::percent}
(all of which are documented at \code{\link[scales]{number_format}}).}

\item{add_plusses}{Logical. Should positive values have plus signs?}

\item{trim_leading_zeros}{Logical. Should \code{signs} trim leading zeros
from values of \code{x} between -1 and 1?}

\item{label_at_zero}{Character. What should be returned
when \code{x = 0}? Options \code{"none"} (no change),
\code{"blank"} (a zero-length string),
or \code{"symbol"} (add a plus-minus symbol).}
}
\value{
A function that takes a numeric vector
  and returns a \code{UTF-8} character vector
}
\description{
Returns a \emph{function} that will format numeric vectors
with proper minus signs.
}
\details{
See \code{\link{signs}} for details.
}
\examples{
x <- seq(-5, 5)
scales::number(x)

f1 <- signs_format()
f1(x)

f2 <- signs_format(accuracy = 1, scale = 1, format = scales::percent)
f2(x)

f3 <- signs_format(add_plusses = TRUE)
f3(x)

f4 <- signs_format(add_plusses = TRUE, label_at_zero = "blank")
f4(x)

f5 <- signs_format(add_plusses = TRUE, label_at_zero = "symbol")
f5(x)

f6 <- signs_format(accuracy = .1, scale = .1, trim_leading_zeros = TRUE)
f6(x)
}
