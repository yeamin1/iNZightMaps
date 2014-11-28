drawContour = function(data, var, var.cond = NULL, loc, baseMap,
                       type, mode, geom, bins,
                       low = NULL, high = NULL, range = NULL, size = NULL) {
    arg <<- as.list(match.call())[-1]
    arg = arg[which(names(arg) %in% c("data", "var", "var.cond", "loc"))]
    dat = do.call(varSubset, arg)
    l = generateLine(type, mode, geom, low, high, range, size)
    cmdLine = paste("baseMap", l, sep = " + ")
    contourMap = eval(parse(text = cmdLine))
    return(contourMap)
}

# TEST ########################################################################
loc = getBB(location = "NZ")
baseMap = setBaseMap(loc, zoom = 5)
drawContour(eq.df, MAG, 2 < eq.df$MAG & eq.df$MAG < 2.1, loc, baseMap, type = "contour", mode = c("fill", "alpha"), geom = "polygon", bins = 4, low = "red", high = "green", range = c(0, 0.5))
drawContour(eq.df, MAG, 2 < eq.df$MAG & eq.df$MAG < 2.1, loc, baseMap, type = "contour", mode = "alpha", geom = "density2d", bins = 4)

loc = getBB(location = "houston")
baseMap = setBaseMap(loc, zoom = 10)
drawContour(crime, hour, 3 <= crime$hour & crime$hour <= 4, loc, baseMap, type = "contour", mode = c("fill", "alpha"), geom = "polygon", bins = 4)
drawContour(crime, hour, 3 <= crime$hour & crime$hour <= 4, loc, baseMap, type = "contour", mode = c("fill", "alpha"), geom = "polygon", bins = 4, low = "red", high = "green")

drawContour(crime, hour, 3 <= crime$hour & crime$hour <= 4, loc, baseMap, type = "contour", mode = "alpha", geom = "density2d", bins = 4, size = 1)

###############################################################################
