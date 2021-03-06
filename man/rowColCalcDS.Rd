\name{rowColCalcDS}
\alias{rowColCalcDS}
\title{Computes sums and means of rows or columns of numeric arrays}
\usage{
rowColCalcDS(dataset, operation)
}
\arguments{
  \item{dataset}{an array of two or more dimensions.}

  \item{operation}{an integer that indicates the operation
  to carry out: 1 for 'rowSums', 2 for 'colSums', 3 for
  'rowMeans' or 4 for 'colMeans'}
}
\value{
a numeric vector
}
\description{
The function is similar to R base functions 'rowSums',
'colSums', 'rowMeans' and 'colMeans'.
}
\details{


the output is returned to the user only the number of
entries in the output vector is greater or equal to the
allowed size.
}
\author{
Gaye, A.
}

