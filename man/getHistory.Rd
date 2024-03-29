% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getHistory.R
\name{getHistory}
\alias{getHistory}
\alias{getHistory,ANY-method}
\alias{getHistory,geom-method}
\alias{getHistory,Raster-method}
\title{Get the history of a spatial object.}
\usage{
getHistory(x, ...)

\S4method{getHistory}{ANY}(x)

\S4method{getHistory}{geom}(x)

\S4method{getHistory}{Raster}(x)
}
\arguments{
\item{x}{the object from which to derive the history.}

\item{...}{other arguments.}
}
\value{
A list of the events that lead to \code{x}.
}
\description{
Get the history of a spatial object.
}
\examples{
library(tibble)
library(magrittr)

geom <- tibble(x = c(40, 70, 70, 50),
               y = c(40, 40, 60, 70)) \%>\%
  gs_polygon() \%>\%
  gt_reflect(angle = 45)
getHistory(x = geom)

getHistory(x = gtRasters)
}
\seealso{
Other getters: 
\code{\link{getCRS}()},
\code{\link{getCols}()},
\code{\link{getExtent}()},
\code{\link{getFeatures}()},
\code{\link{getGroups}()},
\code{\link{getLayers}()},
\code{\link{getNames}()},
\code{\link{getPoints}()},
\code{\link{getRes}()},
\code{\link{getRows}()},
\code{\link{getType}()},
\code{\link{getWindow}()}
}
\concept{getters}
