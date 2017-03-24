\name{gr.pca}
\alias{gr.pca}
\title{
The geometric rotation approach
}
\description{
This algorithm rotates scaled genotypes among family members so that they are mutually orthogonal.
}
\usage{
gr.pca(data.input, index.family, myfam, weight, top, family.size, inflation)
}
\arguments{
  \item{data.input}{
Input dataset, each row is for a genetic feature (SNP), each column is for individual. Data are typically
number of minor alleles, possibly imputed.
}
  \item{index.family}{
Index vector to indicate the family id of each individual.
}
  \item{myfam}{
This value comes directly from the output of findfamilies().
}
  \item{weight}{
Weight is 0 by default. This is a deprecated weight value that can be used to control the amount of rotation performed.  A weight of zero performs full orthogonalization, while a weight of 1 keeps the data unchanged.
}
  \item{top}{
The number of eigenvectors to be used.
}
  \item{family.size}{
The number of members in each family. Used to determine rotation angles.
}
  \item{inflation}{
The inflation of the data value is 0 under default. Deprecated.
}
}

\value{
\item{data.new}{The new datamatrix after the geometric rotation}
\item{topPCs}{The top eigenvectors}
\item{topEigenvalue}{The top eigenvalues.}
}
\references{
Computation of ancestry scores with mixed families and unrelated individuals.  arXiv:1606.08416.
}
\author{
Yi-Hui Zhou
}
