## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(sf)
nc_sf <- st_read(system.file("shape/nc.shp", package="sf"))
st_bbox(nc_sf)

library(sp)
nc_sp <- as_Spatial(nc_sf)
bbox(nc_sp)

library(raster)
ras <- raster(system.file("external/test.grd", package="raster"))
extent(ras)

## ------------------------------------------------------------------------
library(geometr)
myInput <- nc_sf
getExtent(x = myInput)

myInput <- nc_sp
getExtent(x = myInput)

myInput <- ras
getExtent(x = myInput)

## ------------------------------------------------------------------------
# transform from class sf
(nc_geom <- gc_geom(input = nc_sf))

# make "by hand"
library(tibble)
coords <- tibble(x = c(40, 70, 70, 50),
                 y = c(40, 40, 60, 70))
(somePoints <- gs_point(anchor = coords))

## ------------------------------------------------------------------------
aPoly <- gs_polygon(anchor = coords)

getPoints(x = somePoints)
getPoints(x = aPoly)

## ------------------------------------------------------------------------
pond <- tibble(x = c(30, 30, 80, 80, 30),
               y = c(30, 80, 80, 30, 30),
               fid = 1)
island <- tibble(x = c(60, 65, 65, 60, 60),
                y = c(45, 45, 50, 50, 45),
                fid = 1)
temp <- rbind(pond, island, getPoints(aPoly))

perforatedPoly <- gs_polygon(anchor = temp)
visualise(perforatedPoly, fillcol = fid)

## ------------------------------------------------------------------------
getTable(x = nc_geom, slot = "point")

getTable(x = nc_geom, slot = "feature")

getTable(x = nc_geom, slot = "group")

## ------------------------------------------------------------------------
new_geom <- gc_geom(input = nc_sf, group = TRUE)

getTable(x = new_geom, slot = "feature")

getTable(x = new_geom, slot = "group")

## ------------------------------------------------------------------------
library(readr)
library(magrittr)
clocaenog <- read_delim(file = "http://www.pommerening.org/wiki/images/d/dc/Clg6.txt", 
                        delim = "\t", col_types = "iifdddd")

# make geom of locations ...
locations <- tibble(x = clocaenog$x,
                    y = clocaenog$y,
                    fid = clocaenog$Number)
locations <- gs_point(anchor = locations)

# ... and a table of tree properties
trees <- tibble(fid = clocaenog$Number,
                species = clocaenog$Species,
                dbh = clocaenog$dbh,
                height = clocaenog$ht)

# set these as attribute table and select only trees with a height value
trees <- setTable(x = locations, table = trees) %>% 
  getSubset(!is.na(height))

# make the title
treepoints <- getTable(x = trees, slot = "point")
title <- paste0("Clocaenog - tree heights (", dim(treepoints)[1], ")")

# and visualise
visualise(!!title := trees, linecol = height)

## ------------------------------------------------------------------------
visualise(aPoly)

reflPoly <- gs_polygon(anchor = somePoints) %>% 
  gt_reflect(angle = 45)

visualise(reflPoly, linecol = "deeppink", new = FALSE, trace = TRUE)

## ------------------------------------------------------------------------
# make points and discard duplicates ...
(somePoints <- gs_point(anchor = aPoly) %>% 
  getSubset(!(duplicated(x) & duplicated(y)), slot = "point"))

# ... and recreate (the same) polygon from that 
(aPoly <- gs_polygon(anchor = somePoints))

## ------------------------------------------------------------------------
visualise('I made a polygon!' = aPoly, gtRasters$continuous)

## ------------------------------------------------------------------------
relPoly <- gs_rectangle(anchor = aPoly) %>% 
  setWindow(to = tibble(x = c(0, 100), y = c(0, 100))) %>%
  gt_scale(to = "relative")

visualise(gtRasters$categorical, gtRasters$continuous)
visualise(relPoly, linecol = "deeppink", new = F)

## ------------------------------------------------------------------------
zoom <- setWindow(x = relPoly, to = getExtent(x = gtRasters$categorical)) %>% 
  gt_scale(to = "absolute")
visualise('zoomed object' = gtRasters$categorical, window = getExtent(x = zoom))

## ------------------------------------------------------------------------
anImage <- brick(system.file("external/rlogo.grd", package="raster"))
visualise('R logo' = anImage, image = TRUE)

## ------------------------------------------------------------------------
gtTheme

## ------------------------------------------------------------------------
treeTheme <- setTheme(vector = list(scale = list(x = "pointsize", to = "dbh"), 
                                    pointsize = c(0.05, 0.77)))
visualise(`Clocaenog - tree diameters` = trees, theme = treeTheme)

## ------------------------------------------------------------------------
treeTheme <- setTheme(vector = list(pointsize = c(0.05, 0.77)))
visualise(`Clocaenog - tree diameters and heights` = trees, theme = treeTheme,
          pointsize = dbh, linecol = height)

## ------------------------------------------------------------------------
treeTheme <- setTheme(vector = list(linecol = c("#00204DFF", "#73D216FF", "#FFEA46FF", "#CC0000F0")))
visualise(`Clocaenog - tree heights` = trees, theme = treeTheme, linecol = height)


