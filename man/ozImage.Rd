\name{ozImage}
\alias{ozImage}
\title{Create an Image on a Map of Australia}
\description{Creates a map of Australia (or part thereof),
  plus an \code{imageGrob} positioned relative to the map.}
\usage{
ozImage(mapLong, mapLat, imageLong, imageLat, cols) 
}
\arguments{
  \item{mapLong, mapLat}{Longitude and latitude ranges describing the
    area of Australia to create.}
  \item{imageLong, imageLat}{Longitude and latitude ranges describing the
    area that the image should occupy.}
  \item{cols}{Colors for the image cells.}
}
\keyword{dplot}
