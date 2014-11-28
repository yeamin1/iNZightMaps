library(ggmap)
library(ggplot2)
library(animation)

draw = function(data, var, var.cond = NULL,
                location, zoom = 5, maptype = "terrain", src = "google",
                animate = FALSE, n = 5,
                type = "point", mode = "colour",
                geom = NULL, bins, low = NULL, high = NULL, range = NULL, size = NULL,
                title = NULL, legend.title = NULL) {
    
    loc = getBB(location)
    baseMap = setBaseMap(loc, zoom, maptype, src)
    pars = as.list(match.call())[-1]
    arg = c("data", "var", "var.cond", "type", "mode")
    
    if (type == "point") {
        pArg = pars[which(names(pars) %in% arg)]
        pArg$loc = loc
        pArg$baseMap = baseMap
        map = do.call(drawPoints, pArg)
    } else if (type == "contour")
        map = drawContour(data, var, var.cond, loc, baseMap, type, mode, 
                          geom, bins, low, high, range, size)
    else if (type == "both")
        map = drawPoints(data, var, var.cond, loc, baseMap, type, mode) +
        drawContour(data, var, var.cond, loc, baseMap, type, mode, 
                    geom, bins, low, high, range, size)
    
    map
    
#     if (length(legend.title) != 0) {
#         map + xlab(label = "Longitude") + ylab(label = "Latitude") +
#             guides(colour = guide_legend(var))
#     } else 
#         map + xlab(label = "Longitude") + ylab(label = "Latitude")
}

# TEST ########################################################################
data = read.csv("//Users/eric/Desktop/Plotting on Maps/Earthquakes on NZ Map/NZ_earthquakes2010.csv")
draw(data, MAG, 4 < data$MAG & data$MAG < 5, location = "NZ", zoom = 5, maptype = "terrain", src = "google", type = "point", mode = c("size", "colour"))

data(crime)
draw(crime, hour, crime$hour <= 1  & crime$hour > 0, location = "houston", zoom = 10, maptype = "terrain", src = "google", type = "point", mode = c("size", "alpha", "colour"))



#draw(eq.df, MAG, 2 < eq.df$MAG & eq.df$MAG < 2.1, type = "point", mode = "colour", location = "NZ")
# draw(eq.df, MAG, 2 < MAG & MAG < 2.1, type = "point", mode = c("size", "colour", "alpha"), location = "NZ")
# 
# draw(eq.df, "MAG", "2 < MAG & MAG < 2.1", type = "contour", mode = "fill", location = "NZ", geom = "density2d", bins = 4)
# draw(eq.df, "MAG", "2 < MAG & MAG < 2.1", type = "contour", mode = "fill", location = "NZ", geom = "polygon", bins = 4)
# 
# draw(eq.df, "MAG", "2 < MAG & MAG < 2.1", type = "both", mode = c("size", "colour", "alpha", "fill"), geom = "density2d", bins = 4, location = "NZ")

###############################################################################
