\name{xmm}
\docType{data}
\alias{xmm}
\title{X-ray Multi-Mirror space telescope}
\description{
  These data give information on
  applications for time slots on the
  European Space Agency's  X-ray Multi-Mirror space telescope.
}
\usage{
xmm
}
\format{A data frame with columns:
  \describe{
    \item{Category}{
      The proposal category, which describes what sort of
      object will be viewed (a label from \code{A} to \code{G}).
    }
    \item{Priority}{
      The assessed priority of the proposal (a label from
      \code{A} to \code{C}).
    }
    \item{Schedule}{
      Whether the proposal dictates a specific time (\code{fixed})
      or can be conducted at any time (\code{free}).
    }
    \item{Duration}{
      The proposed total observation time, in seconds.
    }
    \item{nObs}{
      Whether the proposal is for a \code{single} observation
      or for \code{multiple} observations.
    }
  }
}
\references{
  The data were obtained from the XMM web site
  \url{http://xmm.esac.esa.int/}.
}
\keyword{datasets}
