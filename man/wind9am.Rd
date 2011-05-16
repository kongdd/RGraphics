\name{wind9am}
\docType{data}
\alias{wind9am}
\title{Auckland Wind Data}
\description{
  These data give measurements of wind speed and direction
  at several weather stations located around Auckland New Zealand.
  The measurements are daily recordings taken at 9:00am each day
  spanning a period of
  approximately two years (September 2008 to September 2010).
}
\usage{
wind9am
}
\format{A data frame with columns:
  \describe{
    \item{Station}{
      A unique identifier for each weather station.
    }
    \item{Date}{
      A Date-Time for each observation (essentially just the day).
    }
    \item{Speed}{
      The wind speed.
    }
    \item{Dir}{
      The wind direction (in degrees).
    }
  }
}
\references{
  The data were obtained from
   the New Zealand National Climate Database
   (\url{http://cliflo.niwa.co.nz/}).
}
\keyword{datasets}
