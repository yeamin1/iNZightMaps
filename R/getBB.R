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
    cat("geocoding location... ")
    currentTime = Sys.time()
    loc = suppressMessages(geocode(location, output = "more"))
    completeTime = Sys.time()
    timeCat(currentTime, completeTime)
    class(loc) = c(class(loc), "geoBBox")
    return(loc)
}
