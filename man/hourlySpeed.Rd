\name{hourlySpeed}
\docType{data}
\alias{hourlySpeed}
\title{Auckland Wind Data}
\description{
  These data give measurements of
   hourly average wind speed
   based on data from 11 weather stations
   located around Auckland, New Zealand.  There are hourly readings
   every day for 
   one month (September 2010). 
}
\usage{
hourlySpeed
}
\format{A data frame with columns:
  \describe{
    \item{Speed}{
      The wind speed.
    }
    \item{day}{
      Day of the year, from 237 to 271.
    }
    \item{hour}{
      Hour of the day, from 0 to 23.
    }
  }
}
\references{
  The data were obtained from
   the New Zealand National Climate Database
   (\url{http://cliflo.niwa.co.nz/}).
}
\keyword{datasets}
