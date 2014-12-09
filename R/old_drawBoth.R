drawBoth = function(data, var, var.cond = NULL, loc, baseMap,
                    type = "both", geom = "density2d", bins = NULL,
                    mode.p = NULL, size.p = NULL, col.p = NULL,
                    mode.c = NULL, size.c = NULL, col.c = NULL,
                    low.p = NULL, high.p = NULL, cols.p = NULL,
                    low.c = NULL, high.c = NULL, cols.c = NULL,
                    title = NULL, grid = NULL, solid = FALSE) {
    allArg = as.list(match.call())[-1]
    arg = allArg[which(names(allArg) %in% c("data", "var", "var.cond", "loc"))]
    
    factor = FALSE
    if (yesFactor <- isFactor(arg$var)) {
        arg$var = as.name(arg[["var"]][[-1]])        
        factor = TRUE
    }
    
    dat = do.call(varSubset, arg)
    l = generateLine(type = "both", geom = geom, grid = grid,
                     mode.p = mode.p, size.p = size.p, col.p = col.p,
                     mode.c = mode.c, size.c = size.c, col.c = col.c,
                     low.p = low.p, high.p = high.p, cols.p = cols.p,
                     low.c = low.c, high.c = high.c, cols.c = cols.c)
    
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
    map = eval(parse(text = cmdLine))
    return(map)
}
