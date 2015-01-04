drawMap = function(location, zoom = 10,
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
    
    if (inherits(location, "geoBBox"))
        loc = location
    else if (is.character(location)) {
        loc = getBB(location)
        if (is.null(loc))
            stop("location not found")
    } else if (length(location) == 2 & all(is.finite(location)))
        loc = data.frame(loc = location[1], lat = location[2])
    
    if (loc$type == "country" & zoom == 10)
        zoom = 5
    
    cat("retrieving map... ")
    currentTime = Sys.time()
    
    baseMapRaster = suppressMessages(
        get_map(location = c(lon = loc$lon, lat = loc$lat), zoom = zoom, 
                maptype = maptype, source = src, color = colour)
    )
    
    completeTime = Sys.time()
    
    baseMap = ggmap(baseMapRaster)
    
    timeCat(currentTime, completeTime)
    return(baseMap)
}
