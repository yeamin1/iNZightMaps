drawPoints = function(data, var, var.cond = NULL, loc, baseMap, type, mode) {
    arg = as.list(match.call())[-1]
    arg[which(names(arg) %in% c("baseMap", "type", "mode"))] = NULL
    dat = do.call(varSubset, arg)
    l = generateLine(type, mode)
    var = deparse(substitute(var))
    cmdLine = paste("baseMap", l, sep = " + ")
    pointMap = eval(parse(text = cmdLine))
    return(pointMap)
}

# TEST ########################################################################
loc = getBB(location = "NZ")
baseMap = setBaseMap(loc, zoom = 5)
a = varSubset(eq.df, MAG, loc = loc)
drawPoints(eq.df, MAG, 4 < eq.df$MAG & eq.df$MAG < 5, loc, baseMap, type= "point", mode = c("size","colour", "alpha"))

loc = getBB(location = "houston")
baseMap = setBaseMap(loc, zoom = 10)
drawPoints(crime, hour, 3 <= crime$hour & crime$hour <= 4, loc, baseMap, type = "point", mode = c("colour", "alpha"))
###############################################################################
