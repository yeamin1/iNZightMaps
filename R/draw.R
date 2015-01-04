draw = function(data, lon = NULL, lat = NULL, var, var.cond = NULL,
                location, zoom = NULL, maptype = "terrain", src = "google",
                type, mode = NULL, mode.p = NULL, mode.c = NULL,
                geom = NULL, bins = NULL, grid = NULL,
                low = NULL, low.p = NULL, low.c = NULL,
                high = NULL, high.p = NULL, high.c = NULL,
                size = NULL, size.p = NULL, size.c = NULL,
                col = NULL, col.p = NULL, col.c = NULL,
                cols = NULL, cols.p = NULL, cols.c = NULL,
                solid = FALSE, title = NULL) {
    allArg = as.list(match.call())[-1]
    arg = allArg[which(names(allArg) %in% 
                           c("data", "lon", "lat", "var", "var.cond"))]
    arg$loc = getBB(location)
    
    factor = FALSE
    if (yesFactor <- isFactor(arg$var)) {
        arg$var = as.name(arg[["var"]][[-1]])        
        factor = TRUE
    }
        
    dat = do.call(varSubset, arg)
    
    l = generateLine(type, mode, mode.p, mode.c, geom, grid, factor,
                     low, low.p, low.c, high, high.p, high.c,
                     size, size.p, size.c, col, col.p, col.c,
                     cols, cols.p, cols.c, solid)
    
    if (yesFactor) {
        l = processFactor(arg$var, l, mode)
        if (any(grepl("shape", mode)))
            n = length(unique(dat[, names(dat) == arg$var]))
        
        if (isNrow(grid) | isNcol(grid))
            grid = processGrid(grid)
    }
    
    ###########################################################################
    # COULD FIX HERE TO MAKE PRETTY TITLES (a new method for generating titles)
    # if (is.null(title))
    #     title = deparse(substitute(var.cond))
    ###########################################################################
    
    baseMap = drawMap(arg$loc, zoom, maptype, src)
    
    var = as.character(arg$var)
    cmdLine = paste("baseMap", l,
                    paste("labs(title = title,", "x = \"Longitude\",",
                          "y = \"Latitude\")"), sep = " + ")
    
    cat("mapping data... ")
    currentTime = Sys.time()
    map = suppressMessages(eval(parse(text = cmdLine)))
    completeTime = Sys.time()
    timeCat(currentTime, completeTime)
    
    return(map)
}
