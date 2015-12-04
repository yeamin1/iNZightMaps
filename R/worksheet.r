data.1 <- read.csv("C:/Users/jason/Desktop/inzight/iNZightVIT/data/QuakesNZ2000.csv", header = TRUE)
devtools::load_all("C:/Users/jason/Desktop/inzight/iNZightPlots-dev-plotmethods/R/iNZightPlots", export_all = FALSE)
devtools::load_all("C:/Users/jason/Desktop/inzight/iNZightMaps-dev-integrateplots/R/iNZightMaps",export_all = FALSE)
library(RgoogleMaps)
library(grid)
source('C:/Users/jason/Desktop/inzight/stupid_loading_method.r')
iNZightPlot(Longitude, Latitude, data = data.1, plottype = "map", xlab="", ylab="")

rr = ratio.transform(maps$window.odd, maps$window.new)
newbbox(maps$usr,rr)
x = newbbox(maps$usr,rr)[1:2]
y = newbbox(maps$usr,rr)[3:4]
new.bb = XY2LatLon(maps$map, x, y, zoom = maps$map$zoom)
map = GetMap.bbox(new.bb[1:2]+c(-0.1,0.1),new.bb[3:4]+c(-0.1,0.1),MINIMUMSIZE = TRUE,SCALE = 2)

grid.raster(maps$map$myTile)


iNZightPlot(Longitude, Latitude, g1=Felt, data = data.1, plottype = "map", colby = Magnitude)



iNZightPlot(Longitude, Latitude, g1=Felt, data = data.1, colby = Magnitude)

grid.layout


grid.newpage()
pushViewport(viewport(0.2,0.2))
grid.raster(maps$map$myTile)
