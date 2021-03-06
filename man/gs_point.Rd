% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gs_point.R
\name{gs_point}
\alias{gs_point}
\title{Create a point \code{geom}}
\usage{
gs_point(anchor = NULL, window = NULL, vertices = 1, ...)
}
\arguments{
\item{anchor}{[\code{geom(1)}|\code{data.frame(1)}]\cr Object to derive the
\code{geom} from. It must include column names \code{x}, \code{y} and
optionally a custom \code{fid}.}

\item{window}{[\code{data.frame(1)}]\cr in case the reference window deviates
from the bounding box of \code{anchor} (minimum and maximum values),
specify this here.}

\item{vertices}{[\code{integer(1)}]\cr number of vertices.}

\item{...}{[various]\cr graphical parameters to \code{\link{gt_locate}}, in
case points are sketched; see \code{\link[grid]{gpar}}}
}
\value{
A \code{geom}.
}
\description{
Create a point geometry (of class \code{\link{geom}}) either by specifying
anchor values or by sketching it.
}
\details{
The argument \code{anchor} indicates how the geom is created:
  \itemize{ \item if \code{anchor} is set, the geom is created parametrically
  from the points given in \code{anchor}, \item if it is not set either
  \code{window} or a default window between 0 and 1 is opened to sketch the
  geom.}
}
\examples{
# 1. create points programmatically
coords <- data.frame(x = c(40, 70, 70, 50),
                     y = c(40, 40, 60, 70))

# if no window is set, the bounding box will be set as window
(aGeom <- gs_point(anchor = coords))

# the vertices are plottet relative to the window
window <- data.frame(x = c(0, 80),
                     y = c(0, 80))
points <- gs_point(anchor = coords, window = window)
visualise(points, linecol = "green")

# when a geom is used in 'anchor', its properties are passed on
aGeom <- setWindow(x = aGeom, to = window)
points <- gs_point(anchor = aGeom)
visualise(points)

# 2. sketch two points
if(dev.interactive()){
  points <- gs_point(vertices = 2)
  visualise(points, linecol = "green", pointsymbol = 5, new = FALSE)
}
}
\seealso{
Other geometry shapes: 
\code{\link{gs_line}()},
\code{\link{gs_polygon}()},
\code{\link{gs_random}()}
}
\concept{geometry shapes}
