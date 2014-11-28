isGeoData = function(data) {
    ret = TRUE
    if (hasLon(data) & hasLat(data)) {
        if (isLon(data) & isLat(data))
            return
    } else {
        stop("No longitude and latitude in data")
        ret = FALSE
    }
    return(ret)
}

## GETTERS
getLon = function(data) { colnames(data)[grepl("^[Ll][Oo][Nn].*$", colnames(data))] }
getLat = function(data) { colnames(data)[grepl("^[Ll][Aa][Tt].*$", colnames(data))] }

## hasLon() & hasLat() check whether data contains variables of longitude and
## latitude, respectively, and return logical.
hasLon = function(data) { length(getLon(data)) > 0 }
hasLat = function(data) { length(getLat(data)) > 0 }

isLon = function(data) {
    # Check whehter longitude values are appropriate.
    # Returns:
    #   Logical.
    values = data[, getLon(data)]
    ret = TRUE
    if(any(!is.finite(values)) & any(values < -180) & any(values > 180)) {
        ret = FALSE
        stop("Longitude must be between -180 and 180.")
    }
    return(ret)
}

isLat = function(data) {
    # Check whehter latitude values are appropriate.
    # Returns:
    #   Logical.
    values = data[, getLon(data)]
    ret = TRUE
    if(any(!is.finite(values)) & any(values < -90) & any(values > 90)) {
        ret = FALSE
        stop("Latitude must be between -90 and 90.")
    }
    return(ret)
}
