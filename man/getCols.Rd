% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getCols.R
\name{getCols}
\alias{getCols}
\alias{getCols,ANY-method}
\alias{getCols,geom-method}
\alias{getCols,Raster-method}
\alias{getCols,matrix-method}
\title{Get the number of columns of a spatial object.}
\usage{
getCols(x, ...)

\S4method{getCols}{ANY}(x)

\S4method{getCols}{geom}(x)

\S4method{getCols}{Raster}(x)

\S4method{getCols}{matrix}(x)
}
\arguments{
\item{x}{the object from which to get the number of columns.}

\item{...}{other arguments.}
}
\value{
An integer of the number of columns.
}
\description{
Get the number of columns of a spatial object.
}
\examples{
getCols(x = gtGeoms$grid$continuous)

getCols(x = gtRasters$categorical)

getCols(x = matrix(0, 3, 5))
}
\seealso{
Other getters: 
\code{\link{getCRS}()},
\code{\link{getExtent}()},
\code{\link{getFeatures}()},
\code{\link{getGroups}()},
\code{\link{getHistory}()},
\code{\link{getLayers}()},
\code{\link{getNames}()},
\code{\link{getPoints}()},
\code{\link{getRes}()},
\code{\link{getRows}()},
\code{\link{getType}()},
\code{\link{getWindow}()}
}
\concept{getters}
