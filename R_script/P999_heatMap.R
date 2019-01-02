dat=readRDS("C:/DOAAT/certificat/project-ds-master/shiny/R_data/airbnb.RDS") %>% as.data.table


kde <- bkde2D(dat[ , list(longitude, latitude)],
              bandwidth=c(.0045, .0068), gridsize = c(100,100))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

## EXTRACT CONTOUR LINE LEVELS
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

## CONVERT CONTOUR LINES TO POLYGONS
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)

## Leaflet map with polygons
leaflet(spgons) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])


##### initi
## INITIALIZE
library("leaflet")
library("data.table")
library("sp")
library("rgdal")
# library("maptools")
library("KernSmooth")

inurl <- "https://data.cityofchicago.org/api/views/22s8-eq8h/rows.csv?accessType=DOWNLOAD"
infile <- "mvthefts.csv"

## LOAD DATA
## Also, clean up variable names, and convert dates
if(!file.exists(infile)){
  download.file(url = inurl, destfile = infile)
}
dat <- data.table::fread(infile)
setnames(dat, tolower(colnames(dat)))
setnames(dat, gsub(" ", "_", colnames(dat)))
dat <- dat[!is.na(longitude)]
dat[ , date := as.IDate(date, "%m/%d/%Y")]

## MAKE CONTOUR LINES
## Note, bandwidth choice is based on MASS::bandwidth.nrd()
kde <- bkde2D(dat[ , list(longitude, latitude)],
              bandwidth=c(.0045, .0068), gridsize = c(100,100))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

## EXTRACT CONTOUR LINE LEVELS
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

## CONVERT CONTOUR LINES TO POLYGONS
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)

## Leaflet map with polygons
leaflet(spgons) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])