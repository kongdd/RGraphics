\name{ozKey}
\alias{ozKey}
\title{Create a Key for an ozImage}
\description{Creates a map of Australia with a rectangle
  bounding a specified region;  designed to provide a key 
  for an \code{ozImage}.}
\usage{
ozKey(x, y, width, height, just, mapLong, mapLat) 
}
\arguments{
  \item{x, y, width, height, just}{
    The location and size of the key within the current viewport.}
  \item{mapLong, mapLat}{
    Longitude and latitude ranges giving the area around which
    to draw a rectangle.}
}
\keyword{dplot}
