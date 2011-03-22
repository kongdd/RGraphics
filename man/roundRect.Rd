\name{roundRect}
\alias{roundRect}
\alias{grid.roundRect}
\title{Draw a rectangle with rounded corners}
\description{Draw a rectangle with rounded corners.}
\usage{
roundRect(x=unit(0.5, "npc"), 
          y=unit(0.5, "npc"), 
          width=unit(1, "npc"), 
          height=unit(1, "npc"), 
          r=unit(0.1, "snpc"),
          just="centre",
          name=NULL, gp=NULL, vp=NULL) 
grid.roundRect(...) 
}
\arguments{
  \item{x, y, width, height}{The location and size of the rectangle.}
  \item{r}{The radius of the rounded corners.}
  \item{just}{The justification of the rectangle relative to its
location.}
  \item{name}{A name to identify the grob.}
  \item{gp}{Graphical parameters to apply to the grob.}
  \item{vp}{A viewport object or \code{NULL}.}
  \item{\dots}{Arguments to be passed to \code{roundRect}.}
}
\keyword{dplot}
