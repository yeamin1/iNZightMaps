drawMap = function(location, zoom = NULL,
                      maptype = "terrain", src = "google",
                      colour = "color") {
    # Retrieve maps from GIS services.
    # Its main use is to set a base map on which data can be drawn.
    # Args:
    #   location: character or object from getBB()
    #   zoom, maptype & src: See ?get_map
    #   colour: "color" by default. Could be "bw".
    # Return:
    #   map of ggplot object
    if (inherits(location, "boundingBox"))
        loc = location
    else if (is.character(location))
        loc = getBB(location)
    else if (length(location) == 2 & all(is.finite(location)))
        loc = data.frame(loc = location[1], lat = location[2])
    
    if (is.null(zoom)) {
        if (loc$type == "country")
            zoom = 5
        else
            zoom = 10
    }
    
    baseMapRaster = get_map(location = c(lon = loc$lon, lat = loc$lat),
                            zoom = zoom, maptype = maptype, source = src,
                            color = colour)
    baseMap = ggmap(baseMapRaster)
    return(baseMap)
}

## GETTER for bounding box
getBB = function(location) {
    # Retrieve the bounding box (ie, boundaries) of location specified.
    # Its main use is to check whether a data set has any location values 
    # outside the boundaries of the location.
    # Args:
    #   location: character
    # Return:
    #   data frame
    # Example:
    #   > getBB("Auckland University")
    loc = geocode(location, output = "more");
    class(loc) = c(class(loc), "boundingBox")
    return(loc)
}


# asFactor = function(data, var) {
#     if (!is.factor(data$var) & ) {   
#     }
#     if (c = is.character(data$var) || f = is.factor(data$var)) {
#         if(c & length(unique(data$var)) < 0.3 * nrow(data)) {   
#         }   
#     }
# }
