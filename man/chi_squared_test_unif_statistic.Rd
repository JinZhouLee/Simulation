% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chi_squared_test_unif_statistic.R
\name{chi_squared_test_unif_statistic}
\alias{chi_squared_test_unif_statistic}
\title{Chi-squared goodness-of-fit test for uniform distribution (0, 1)}
\usage{
chi_squared_test_unif_statistic(x, k = 100)
}
\arguments{
\item{x}{numerical vector: the data}

\item{k}{positive integer: partition number, recommend n / k > 5, where n is the length of x}
}
\value{
test statistic
}
\description{
Chi-squared goodness-of-fit test for uniform distribution (0, 1)
}
\examples{
# x = unif_gen(10000)
# chi_squared_test_unif_statistic(x)
}
\keyword{internal}
