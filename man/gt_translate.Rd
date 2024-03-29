% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gt_translate.R
\name{gt_translate}
\alias{gt_translate}
\title{Translate geometric objects}
\usage{
gt_translate(obj, x = NULL, y = NULL, fid = NULL, update = TRUE)
}
\arguments{
\item{obj}{[\code{geometric object(1)}]\cr the object to translate.}

\item{x}{[\code{numeric(1)}]\cr the translation constant (offset) in x
dimension.}

\item{y}{[\code{numeric(1)}]\cr the translation constant (offset) in y
dimension.}

\item{fid}{[\code{integerish(.)}]\cr in case only a subset of features shall
be translated, specify that here.}

\item{update}{[\code{logical(1)}]\cr whether or not to update the window slot
after translation.}
}
\value{
\code{geom} of the mathematically translated \code{obj}.
}
\description{
Translate geometric objects by adding a constant in x and y dimension.
}
\examples{
# translate several features
visualise(gtGeoms$polygon, linewidth = 3)
newPoly <- gt_translate(obj = gtGeoms$polygon, x = 5, y = c(-10, 5),
                        update = FALSE)
visualise(geom = newPoly, linecol = "green", new = FALSE)

# translate a single feature
visualise(gtGeoms$polygon, linewidth = 3)
newPoly <- gt_translate(obj = gtGeoms$polygon, x = 5, fid = 2,
                        update = FALSE)
visualise(geom = newPoly, linecol = "green", new = FALSE)
}
\seealso{
Other geometry tools: 
\code{\link{gt_filter}()},
\code{\link{gt_locate}()},
\code{\link{gt_pull}()},
\code{\link{gt_reflect}()},
\code{\link{gt_rotate}()},
\code{\link{gt_scale}()},
\code{\link{gt_skew}()},
\code{\link{gt_stretch}()}
}
\concept{geometry tools}
