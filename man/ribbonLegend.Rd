\name{ribbonLegend}
\alias{ribbonLegend}
\title{Create a Ribbon Legend}
\description{Creates a ribbon legend; a vertical bar broken into
  several colored cells, with an axis on the right-hand side.}
\usage{
ribbonLegend(nlevels=NULL, breaks=NULL, cols, 
             scale=range(breaks), 
             margin=unit(0.5, "line"), 
             gp=NULL, vp=NULL, name=NULL) 
}
\arguments{
  \item{nlevels, breaks}{Number of levels to be represented in the legend,
    specifed either as a number of (equal-sized) levels, or the break points
    between levels.}
  \item{cols}{The colors to be used for each level.}
  \item{scale}{The range of the scale on the legend.}
  \item{margin}{Space around the edges of the legend.}
  \item{name}{A character name for the grob.}
  \item{gp}{A \code{gpar} object containing graphical parameter settings
    or \code{NULL}.}
  \item{vp}{A viewport or \code{NULL}.}  
}
\keyword{dplot}
