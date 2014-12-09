varSubset = function(data, var, var.cond = NULL, loc) {
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
    keepCols = c(getLon(data), getLat(data), var)
    completeRows = complete.cases(data[, c(getLon(data), getLat(data), var)])
    
    if (inherits(loc, "geoBBox") |
            sum(grepl("west|east|south|north", colnames(loc))) == 4) {
        loc.cond = loc$west <= data[, getLon(data)] & 
            data[, getLon(data)] <= loc$east &
            loc$south <= data[, getLat(data)] &
            data[, getLat(data)] <= loc$north
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
