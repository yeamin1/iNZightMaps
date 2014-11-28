varSubset = function(data, var, var.cond = NULL, loc) {
    var = deparse(substitute(var))
    if (grepl("c[(]|,", var)) {
        var = gsub("\\s+|^c[(]|[)]$", "", var)
        var = unlist(strsplit(var, ","))
    }
    keepCols = c(getLon(data), getLat(data), var)
    completeRows = complete.cases(data[, c(getLon(data), getLat(data), var)])
    
    if (!is.null(var.cond))
        dat = data[completeRows & var.cond, keepCols]
    else
        dat = data[completeRows, keepCols]
    
    if (nrow(dat) == 0)
        stop(paste("No observations in data frame.",
                   "(check 'var.cond' and/or 'location')", sep = "\n"))
    
    colnames(dat)[1:2] = c("lon", "lat")
    
    return(dat)
}

# TEST ########################################################################
a = varSubset(eq.df, MAG, eq.df$MAG < 5, loc = loc)
a = varSubset(crime, hour, crime$hour < 1, loc = getBB("houston"))

