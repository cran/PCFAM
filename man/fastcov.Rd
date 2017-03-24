\name{fastcov}
\alias{fastcov}
\title{
Fast covariance function
}
\description{
This function can generate covariance matrix faster than the regular cov() function.
}
\usage{
fastcov(X)
}

\arguments{
  \item{X}{
input mxn data matrix
}
}

\value{
Output nxn covariance matrix
}
\references{
Computation of ancestry scores with mixed families and unrelated individuals.  arXiv:1606.08416.
}
\author{
Yi-Hui Zhou,
}
\note{The input data matrix has to be column scaled in advance.
}
