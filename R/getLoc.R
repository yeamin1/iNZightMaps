getLoc = function(data) {
    lon = getLon(data)
    lat = getLat(data)
    
    lonMedian = median(data[!is.na(data[lon]), lon])
    latMedian = median(data[!is.na(data[lat]), lat])
    
    address = suppressMessages(
        revgeocode(location = c(lonMedian, latMedian), output = "more")
    )
    
    return(as.character(address$country))
}
