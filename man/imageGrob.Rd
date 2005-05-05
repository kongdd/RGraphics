\name{imageGrob}
\alias{imageGrob}
\title{Create an Image}
\description{Creates an array of \code{nrow} by \code{ncol} rectangles.}
\usage{
imageGrob(nrow, ncol, cols, byrow=TRUE,
          name=NULL, gp=NULL, vp=NULL) 
}
\arguments{
  \item{nrow, ncol}{Number of rows/columns in the image.}
  \item{cols}{Colours for the cells of the image (will be recycled).}
  \item{byrow}{Logical value indicating whether colours should be
    allocated to cells across rows or down columns.}
  \item{name}{A character name for the grob.}
  \item{gp}{A \code{gpar} object containing graphical parameter settings
    or \code{NULL}.}
  \item{vp}{A viewport or \code{NULL}.}
}
\keyword{dplot}
