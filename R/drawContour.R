drawContour = function(data, var, var.cond = NULL, loc, baseMap,
                       type = "contour", mode = NULL, 
                       geom = "polygon", bins = NULL,
                       low = NULL, high = NULL, col = NULL, cols = NULL,
                       size = NULL, title = NULL, grid = NULL) {
    arg = as.list(match.call())[-1]
    arg = arg[which(names(arg) %in% c("data", "var", "var.cond", "loc"))]
    
    factor = FALSE
    if (yesFactor <- isFactor(arg$var)) {
        arg$var = as.name(arg[["var"]][[-1]])
        factor = TRUE
    }
    
    dat = do.call(varSubset, arg)
    l = generateLine(type = type, mode = mode, geom = geom, 
                     low = low, high = high, col = col, 
                     factor = factor, grid = grid, cols = cols,
                     size = size)
    
    if (yesFactor) {
        l = processFactor(arg$var, l, mode)
        if (any(grepl("shape", mode)))
            n = length(unique(dat[, names(dat) == arg$var]))
        
        if (isNrow(grid) | isNcol(grid))
            grid = processGrid(grid)
    }
    
    ###########################################################################
    # COULD FIX HERE TO MAKE PRETTY TITLES (a new method for generating titles)
    if (is.null(title))
        title = deparse(substitute(var.cond))
    ###########################################################################
        
    var = as.character(arg$var)
    cmdLine = paste("baseMap", l, 
                    paste("labs(title = title,", "x = \"Longitude\",",
                          "y = \"Latitude\")"), sep = " + ")
    contourMap = eval(parse(text = cmdLine))
    return(contourMap)
}
