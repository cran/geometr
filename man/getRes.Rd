% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getRes.R
\name{getRes}
\alias{getRes}
\alias{getRes,ANY-method}
\alias{getRes,geom-method}
\alias{getRes,Raster-method}
\alias{getRes,matrix-method}
\title{Get the spatial resolution of a spatial object.}
\usage{
getRes(x, ...)

\S4method{getRes}{ANY}(x)

\S4method{getRes}{geom}(x)

\S4method{getRes}{Raster}(x)

\S4method{getRes}{matrix}(x)
}
\arguments{
\item{x}{the object from which to derive the resolution.}

\item{...}{other arguments.}
}
\value{
A vector of two values of the spatial resolution of \code{x} in x and
  y dimension.
}
\description{
Get the spatial resolution of a spatial object.
}
\examples{
getRes(x = gtGeoms$grid$continuous)

getRes(x = gtRasters$categorical)

getRes(x = matrix(0, 3, 5))
}
\seealso{
Other getters: 
\code{\link{getCRS}()},
\code{\link{getCols}()},
\code{\link{getExtent}()},
\code{\link{getFeatures}()},
\code{\link{getGroups}()},
\code{\link{getHistory}()},
\code{\link{getLayers}()},
\code{\link{getNames}()},
\code{\link{getPoints}()},
\code{\link{getRows}()},
\code{\link{getType}()},
\code{\link{getWindow}()}
}
\concept{getters}
