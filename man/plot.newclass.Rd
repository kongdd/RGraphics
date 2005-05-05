\name{plot.newclass}
\alias{plot.newclass}
\title{A Traditional Graphics Function Template}
\description{
  A template that provides a starting point for writing
  a new traditional graphics function.
}
\synopsis{
plot.newclass <- 
  function(x, y=NULL, 
           main="", sub="",
           xlim=NULL, ylim=NULL,
           axes=TRUE, ann=par("ann"),
           col=par("col"),
           ...) 
}
\arguments{
  \item{x, y, main, sub, xlim, ylim, axes, ann, col, ...}
       {See \code{plot.default}}
}
\keyword{hplot} 
