\name{ozGrob}
\alias{ozGrob}
\alias{grid.ozGrob}
\title{Create a Map of Australia}
\description{Creates a map of Australia (or part thereof).
\code{grid.ozGrob} also draws the map.}
\usage{
ozGrob(ozRegion, name=NULL, gp=NULL, vp=NULL) 
grid.ozGrob(...)
}
\arguments{
  \item{ozRegion}{An object created using the 
    \code{ozRegion} function from the \pkg{oz} package.}
  \item{name}{A character name for the grob.}
  \item{gp}{A \code{gpar} object containing graphical parameter settings
    or \code{NULL}.}
  \item{vp}{A viewport or \code{NULL}.}
  \item{\dots}{Arguments to be passed to \code{ozGrob}.}
}
\keyword{dplot}
