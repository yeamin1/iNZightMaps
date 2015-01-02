varSubset = function(data, lon = NULL, lat = NULL, var, var.cond = NULL, loc) {
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
    var = deparse(substitute(var))
    lon = deparse(substitute(lon))
    lat = deparse(substitute(lat))
    
    if (lon == "NULL")
        lon = getLon(data)
    if (lat == "NULL")
        lat = getLat(data)
    
    keepCols = c(lon, lat, var)
    completeRows = complete.cases(data[, c(lon, lat, var)])
    
    if (inherits(loc, "geoBBox") |
            sum(grepl("west|east|south|north", colnames(loc))) == 4) {
        loc.cond = loc$west <= data[, lon] & data[, lon] <= loc$east &
            loc$south <= data[, lat] & data[, lat] <= loc$north
    }
    
    cond = completeRows & loc.cond
    
    if (!is.null(var.cond))
        dat = data[cond & var.cond, keepCols]
    else
        dat = data[cond, keepCols]
    
    if (nrow(dat) == 0)
        stop(paste("No observations in data frame.",
                   "(check 'var.cond' and/or 'location')", sep = "\n"))
    
    colnames(dat)[1:2] = c("lon", "lat")
    
    return(dat)
}
