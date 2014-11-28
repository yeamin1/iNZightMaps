# GETTER for bounding box
getBB = function(location) {
    loc = geocode(location, output = "more")
    class(loc) = c(class(loc), "boundingBox")
    return(loc)
}

# SETTER for base map
setBaseMap = function(loc, zoom = 5, maptype = "terrain", src = "google") {
    
    if (inherits(loc, "boundingBox")) 
        location = c(lon = loc$lon, la 
    
    baseMapRaster = get_map(location = c(lon = loc$lon, lat = loc$lat),
                            zoom = zoom, maptype = maptype, source = src)
    baseMap = ggmap(baseMapRaster)
    return(baseMap)
}

# asFactor = function(data, var) {
#     if (!is.factor(data$var) & ) {   
#     }
#     if (c = is.character(data$var) || f = is.factor(data$var)) {
#         if(c & length(unique(data$var)) < 0.3 * nrow(data)) {   
#         }   
#     }
# }
