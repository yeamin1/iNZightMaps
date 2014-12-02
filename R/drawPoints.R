drawPoints = function(data, var, var.cond = NULL, loc, baseMap,
                      type = "point", mode = NULL,
                      low = NULL, high = NULL, col = NULL, cols = NULL,
                      size = NULL, title = NULL, grid = NULL) {
    # Plot geographic data as points
    # Args:
    #   data: data.frame
    #   var: variable
    #   var.cond: condition
    #   loc: location object from getBB()
    #   baseMap: map object from drawMap()
    #   type: either "point", "contour" or "both"
    #   mode: any combination: c("size", "colour", "alpha")
    #   low: low gradient colour
    #   high: high gradient colour
    #   col: points colour
    #   title: plot title
    # Return:
    #   map as a ggplot object
    # Example:
    #   > drawPoints(eq.df, MAG, eq.df$MAG <= 1, loc = getBB("NZ"), baseMap = drawMap("NZ"), type = "point", mode = c("alpha", "size"))
    arg = as.list(match.call())[-1]
    arg = arg[which(names(arg) %in% c("data", "var", "var.cond", "loc"))]
    
    factor = FALSE
    if (yesFactor <- isFactor(arg$var)) {
        arg$var = as.name(arg$var[[-1]])
        factor = TRUE
    }
    
    dat = do.call(varSubset, arg)
    l = generateLine(type = type, mode = mode, low = low, high = high,
                     col = col, factor = factor, grid = grid, cols = cols,
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
    pointMap = eval(parse(text = cmdLine))
    return(pointMap)
}
