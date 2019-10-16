#' Transform a spatial object to class \code{geom}
#'
#' @param input the object to transform to class \code{geom}.
#' @param group [\code{logical(1)}]\cr should the attributes of multi* features
#'   be grouped, i.e. should the unique values per multi* feature be assigned
#'   into the groups table (\code{TRUE}), or should they be kept as duplicated
#'   per-feature attributes (\code{FALSE}, default)?
#' @param ... additional arguments.
#' @return an object of class \code{geom}
#' @family spatial classes
#' @examples
#' (geomPoints <- gc_geom(input = gtPPP))
#' (geomPoly <- gc_geom(input = gtSF$polygon))
#' (geomLine <- gc_geom(input = gtSP$SpatialLinesDataFrame))
#' @name gc_geom
#' @rdname gc_geom
NULL

# generic ----
#' @rdname gc_geom
#' @name gc_geom
#' @export
if(!isGeneric("gc_geom")){
  setGeneric(name = "gc_geom",
             def = function(input, ...){
               standardGeneric("gc_geom")
             }
  )
}

# Spatial ----
#' @rdname gc_geom
#' @importFrom tibble tibble
#' @export
setMethod(f = "gc_geom",
          signature = "Spatial",
          definition = function(input = NULL, ...){

            theCoords <- getPoints(x = input)
            theData <- getTable(x = input)
            theWindow <- getWindow(x = input)
            theCRS <- getCRS(x = input)

            sourceClass <- class(input)[1]
            if(sourceClass %in% c("SpatialPoints", "SpatialPointsDataFrame", "SpatialPixels", "SpatialPixelsDataFrame")){
              type <- "point"
            } else if(sourceClass %in% c("SpatialMultiPoints", "SpatialMultiPointsDataFrame")){
              type <- "point"
            } else if(sourceClass %in% c("SpatialLines", "SpatialLinesDataFrame")){
              type <- "line"
            } else if(sourceClass %in% c("SpatialPolygons", "SpatialPolygonsDataFrame", "SpatialGrid", "SpatialGridDataFrame")){
              type <- "polygon"
            }
            history <- paste0("geometry was transformed from an object of class '", sourceClass, "'.")

            out <- new(Class = "geom",
                       type = type,
                       point = theCoords,
                       feature = theData,
                       group = tibble(gid = unique(theData$gid)),
                       window = theWindow,
                       scale = "absolute",
                       crs = theCRS,
                       history = list(history))

            return(out)
          }
)

# sf ----
#' @rdname gc_geom
#' @importFrom tibble tibble
#' @importFrom sf st_geometry_type
#' @importFrom dplyr bind_cols
#' @export
setMethod(f = "gc_geom",
          signature = "sf",
          definition = function(input = NULL, group = FALSE, ...){

            theCoords <- getPoints(x = input)
            theData <- getTable(x = input)
            theCRS <- getCRS(x = input)
            theWindow <- getWindow(x = input)

            sourceClass <- st_geometry_type(input)
            if(length(unique(sourceClass)) == 1){
              sourceClass <- unique(sourceClass)
            } else{
              # what happens if a sf-object has different feature-types?
            }
            if(sourceClass %in% c("POINT", "MULTIPOINT")){
              type <- "point"
            } else if(sourceClass %in% c("LINESTRING", "MULTILINESTRING")){
              type <- "line"
            } else if(sourceClass %in% c("POLYGON", "MULTIPOLYGON")){
              type <- "polygon"
            }
            if(sourceClass %in% c("MULTIPOINT", "MULTILINESTRING", "MULTIPOLYGON") & group){
              temp <- theData[-which(colnames(theData) %in% c("fid", "gid"))]
              temp <- temp[!duplicated(temp),]
              theGroups <- bind_cols(gid = 1:dim(temp)[1], temp)
              theData <- theData[c("fid", "gid")]
            } else {
              theGroups <- tibble(gid = unique(theData$gid))
            }
            history <- paste0("geometry was transformed from an sf-object of geometry type '", sourceClass, "'.")

            out <- new(Class = "geom",
                       type = type,
                       point = theCoords,
                       feature = theData,
                       group = theGroups,
                       window = theWindow,
                       scale = "absolute",
                       crs = theCRS,
                       history = list(history))

            return(out)
          }
)

# ppp ----
#' @rdname gc_geom
#' @importFrom tibble tibble
#' @export
setMethod(f = "gc_geom",
          signature = "ppp",
          definition = function(input = NULL, ...){

            theCoords <- getPoints(x = input)
            theData <- getTable(x = input)
            theGroups <- tibble(gid = theData$gid)
            theWindow <- getWindow(x = input)
            history <- paste0("geometry was transformed from an object of class ppp.")
            theCRS <- NA_character_

            out <- new(Class = "geom",
                       type = "point",
                       point = theCoords,
                       feature = theData,
                       group = theGroups,
                       window = theWindow,
                       scale = "absolute",
                       crs = theCRS,
                       history = list(history))

            return(out)
          }
)