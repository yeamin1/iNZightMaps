varSubset = function(data, lon = NULL, lat = NULL, 
                     colour_by = NULL, colour_by_cond = NULL, 
                     size_by = NULL, size_by_cond = NULL, location) {
    # Subset data set according to variable(s) and condition(s) specified.
    # Rows of longitude and latitude outside the bounding box of location are
    # dropped.
    # Args:
    #   data: data.frame
    #   var: variable
    #   var.cond: condition
    #   loc: location
    # Return:
    #   subsetted data frame
    # Example:
    #   > varSubset(eq.df, MAG, 4 <= eq.df$MAG & eq.df$MAG <= 5, loc = getBB("NZ"))
    
    lon = substitute(lon)
    lat = substitute(lat)
    if (!is.null(colour_by)) { colour_by = substitute(colour_by) }
    if (!is.null(size_by)) { size_by = substitute(size_by) }
    
    if (!is.character(lon)) { lon = deparse(lon) }
    if (!is.character(lat)) { lat = deparse(lat) }
    if (!is.null(colour_by) & !is.character(colour_by))
        var = deparse(colour_by)
    if (!is.null(size_by) & !is.character(size_by))
        var = deparse(size_by)
    
    if (lon == "NULL") { lon = getLon(data) }
    if (lat == "NULL") { lat = getLat(data) }
    
    ## ggmap uses [0, 360] range for longitude, instead of [-180, 180]
    data[data$lon < 0, lon] = data[data$long < 0, lon] + 360
    
    keepCols = c(lon, lat, colour_by, size_by)
    completeRows = complete.cases(data[, keepCols])
    
    if (is.null(colnames(location))) {
        colnames(location) = c("north", "east", "south", "west")
    }
    
    westCond = location$west <= data[, lon]
    eastCond = data[, lon] <= location$east
    southCond = location$south <= data[, lat]
    northCond = data[, lat] <= location$north
    
    location_cond = westCond & eastCond & southCond & northCond
    
    # if (inherits(location, "geoBBox") |
    #         sum(grepl("west|east|south|north", colnames(loc))) == 4) {
    # }
    
    cond = completeRows & location_cond
    
    
    if (!is.null(colour_by_cond)) {
        if (is.character(colour_by_cond))
            colour_by_cond = eval(parse(text = colour_by_cond))
        cond = cond & colour_by_cond
    }
    if (!is.null(size_by_cond)) {
        if (is.character(size_by_cond))
            size_by_cond = eval(parse(text = size_by_cond))
        cond = cond & size_by_cond
    }
    
    dat = data[cond, keepCols]
#     if (!is.null(var.cond)) {
#         if (is.character(var.cond))
#             var.cond = eval(parse(text = var.cond))
#         dat = data[cond & var.cond, keepCols]
#     } else {
#         dat = data[cond, keepCols]
#     }
    
    if (nrow(dat) == 0)
        stop("No observations in data frame.")
    
    colnames(dat)[1:2] = c("lon", "lat")
    
    return(dat)
}
