draw = function(data, lon = NULL, lat = NULL, 
                colour_by = NULL, colour_by_cond = NULL,
                size_by = NULL, size_by_cond = NULL,
                location, zoom = 10, maptype = "terrain", 
                src = "google", colour = "color",
                type, mode = NULL, mode.p = NULL, mode.c = NULL,
                geom = NULL, bins = NULL, grid = NULL,
                low = "#5EB7F8", low.p = NULL, low.c = NULL,
                high = "#1A334B", high.p = NULL, high.c = NULL,
                size = NULL, size.p = NULL, size.c = NULL,
                col = NULL, col.p = NULL, col.c = NULL,
                cols = NULL, cols.p = NULL, cols.c = NULL,
                solid = FALSE, title = NULL) {
    
    if (is.null(colour_by) & is.null(size_by))
        stop("colour_by and size_by can't both be null")
    
    allArgs = as.list(match.call())[-1]
    varSubsetArgs = c("data", "lon", "lat", 
                      "colour_by", "colour_by_cond",
                      "size_by", "size_by_cond")
    arg = allArgs[which(names(allArgs) %in% varSubsetArgs)]
    
    if (is.character(location)) {
        arg$loc = getBB(location)
    } else {
        cat("geocoding location... ")
        currentTime = Sys.time()
        arg$loc = data.frame( north = location[1], east = location[2],
                              south = location[3], west = location[4])
        completeTime = Sys.time()
        timeCat(currentTime, completeTime)
    }
    
    # factor = FALSE
    # if (yesFactor <- isFactor(arg$var)) {
    #     arg$var = as.name(arg[["var"]][[-1]])
    #     factor = TRUE
    # }
    
    dat = do.call(varSubset, arg)
    
    l = generateLine(type, mode, mode.p, mode.c, geom, grid, factor,
                     low, low.p, low.c, high, high.p, high.c,
                     size, size.p, size.c, col, col.p, col.c,
                     cols, cols.p, cols.c, solid)
    
    # if (yesFactor) {
    #     l = processFactor(arg$var, l, mode)
    #     if (any(grepl("shape", mode)))
    #         n = length(unique(dat[, names(dat) == arg$var]))
    #     
    #     if (isNrow(grid) | isNcol(grid))
    #         grid = processGrid(grid)
    # }
    
    ###########################################################################
    # COULD FIX HERE TO MAKE PRETTY TITLES (a new method for generating titles)
    # if (is.null(title))
    #     title = deparse(substitute(var.cond))
    ###########################################################################
    
    baseMap = drawMap(arg$loc, zoom, maptype, src, colour)
    
    var = as.character(arg$var)
    cmdLine = paste("baseMap", l,
                    paste("labs(title = title,", "x = \"Longitude\",",
                          "y = \"Latitude\")"), sep = " + ")
    
    cat("mapping data... ")
    currentTime = Sys.time()
    if (is.null(colour_by)) { colour_by = size_by }
    if (is.null(size_by)) { size_by = colour_by }
    map = suppressMessages(eval(parse(text = cmdLine)))
    completeTime = Sys.time()
    timeCat(currentTime, completeTime)
    
    return(map)
}
