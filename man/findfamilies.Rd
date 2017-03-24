\name{findfamilies}
\alias{findfamilies}
\title{
Find families
}
\description{
This function searches for pairs of individuals with high kinship based on the genotype correlation matrix.
}
\usage{
findfamilies(x, threshold = 0.4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The nxn correlation matrix of the input dataset.
}
  \item{threshold}{
This threshold is used to identify close-degree relatives. Recommended values are 0.4 to identify first-degree relatives, and 0.15 to identify first- and second-degree relatives.
}
}

\value{
Output numerical family ID for each individual. Individuals with the same ID are judged to be family members.
}
\references{
Computation of ancestry scores with mixed families and unrelated individuals. arXiv:1606.08416.
}
\author{
Yi-Hui Zhou
}
