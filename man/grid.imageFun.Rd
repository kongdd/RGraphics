\name{grid.imageFun}
\alias{grid.imageFun}
\title{Draw an Image}
\description{Draws an array of \code{nrow} by \code{ncol} rectangles.}
\usage{
grid.imageFun(nrow, ncol, cols, byrow=TRUE) 
}
\arguments{
  \item{nrow, ncol}{Number of rows/columns in the image.}
  \item{cols}{Colors for the cells of the image (will be recycled).}
  \item{byrow}{Logical value indicating whether colors should be
    allocated to cells across rows or down columns.}
}
\keyword{dplot}
