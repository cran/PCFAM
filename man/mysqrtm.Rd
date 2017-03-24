\name{mysqrtm}
\alias{mysqrtm}
\title{
Matrix square root function
}
\description{
This function can find the matrix square root, without requiring a new package and often faster than other code.
}
\usage{
mysqrtm(a, symmetric = F)
}

\arguments{
  \item{a}{
The input matrix
}
  \item{symmetric}{
Default=FALSE. This argument indicates whether the input matrix is symmetric.
}
}
\details{
Matrix B is said to be a square root of A if the matrix product BB is equal to A.
}
\value{
returns the square root matrix B
}
