####Lab 4####
library(OpenStreetMap)
library(rgdal)
library(raster)
#1b####
amyraster<-raster("/Users/maxwell1/Documents/GitHub/Spatial-Wanderings/new_rech.tif", ncols=1000,nrows=1000)
bounds<-read.csv("/Users/maxwell1/Documents/GitHub/Spatial-Wanderings/databounds.csv")
wells<-read.csv("/Users/maxwell1/Documents/GitHub/Spatial-Wanderings/MW_label.csv")
#turnboundsinto spatialpoints
WGScoor<-bounds
str(bounds)
lat<-WGScoor$y
long<-WGScoor$x
coordinates(WGScoor)=~x+y
proj4string(WGScoor)<- CRS("+proj=longlat +datum=WGS84")
raster::shapefile(WGScoor, "/Users/maxwell1/Documents/GitHub/Spatial-Wanderings/MyShapefile.shp", overwrite=TRUE)
amyshape<-readOGR("/Users/maxwell1/Documents/GitHub/Spatial-Wanderings/MyShapefile.shp")

amyshape
amycut<-mask(amyraster,amyshape)
plot(amycut)

map <- openmap (c(38.31263, -121.3913), c(38.29174, -121.3686),type='esri-topo')#provides the map boundary using upper-left and lower-right corners
map_longlat <- openproj(map)
crs(map_longlat)
crs(amycut)<-crs(map_longlat)
par(mar=c(0,0,0,0))
plot(map_longlat)
points(y=wells$lat,wells$long)
text(y=wells$lat,wells$long, labels=wells$name, pos=1, cex=0.75)
plot(amycut,add=TRUE,alpha=0.4, col=rainbow(100, start = 0, end = .7, alpha = 1))
library(maps)
maps::map.scale(y=38.294, x=-121.39, relwidth=0.15, cex=.5)
library(GISTools)
north.arrow(yb=38.294, xb=-121.391, len=0.0002, lab="N")
library(ggplot2)
autoplot(map_longlat)+xlab(c(""))+ylab(c(""))
library(raster)
library(rasterVis)
library(rgdal)

library(tmap)
e<-extent(amycut)
p<-as(e,'SpatialPolygons')
r<-amycut>-Inf
pp<-rasterToPolygons(r,dissolve=TRUE)
plot(amycut)
plot(p, add=T)
plot(pp, add=T)


amyraster
library(quadmesh)
library(rgl)
library(plot3D)
library(lattice)
library(rasterVis)
data(volcano)
r <- raster(volcano)
extent(r) <- c(0, 610, 0, 870)
levelplot(r, col.regions=terrain.colors)
rasterVis::plot3D(amycut,col=terrain.colors(25))

volcanos <- list(r,r)
col <- c("blue", "red")
open3d()
mfrow3d(2,1)
for (i in 1:2) {
  next3d()   
  plot3D(volcanos[[i]], col=col[i])
}

