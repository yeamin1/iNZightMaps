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
    
    varChar = as.character(arg$var)
    factor = FALSE
    if (factorL <- (length(arg$var) == 2 & grepl("^factor$", varChar[1]))) {
        arg$var = as.name(gsub("[(][)]", "", varChar[2]))
        factor = TRUE
    }
    
    dat = do.call(varSubset, arg)
    l = generateLine(type = type, mode = mode, low = low, high = high,
                     col = col, factor = factor, grid = grid, cols = cols,
                     size = size)
    if (factorL) {
        replace = paste0("= \"factor(", as.character(arg$var), ")\"")
        l = gsub("= var", replace, l)
        n = length(unique(dat[, names(dat) == as.character(arg$var)]))
        if (any(grepl("shape", mode)))
            l = paste(l, "scale_shape_manual(values = 1:n)", sep = " + ")
        if (grepl("facet_wrap[(] ~ facetVariable", l))
            l = gsub("facetVariable", as.character(arg$var), l)
    }
    if (is.null(title))
        title = deparse(substitute(var.cond))
    var = as.character(arg$var)
    cmdLine = paste("baseMap", l,
                    paste("labs(title = title,", "x = \"Longitude\",",
                          "y = \"Latitude\")"), sep = " + ")
    
    pointMap = eval(parse(text = cmdLine))
    return(pointMap)
}

###############################################################################
# DO WE WANT TO PLOT 2 VARIABLES ON ONE MAP?
#     start = 1
#     end = 1
#     l = character()
#     varChar = as.character(arg$var)
#     if (grepl("^c$", as.character(arg$var)[1]) & length(arg$var) > 1) {
#         arg$var = arg$var[-1]
#         varChar = varChar[-1]
#         end = length(varChar)
#     }
#     varcChar = as.character(arg$var.cond)
#     if (nCond <- grepl("^c$", as.character(arg$var.cond)[1])
#                  & length(arg$var.cond) > 1) {
#         arg$var.cond = arg$var.cond[-1]
#         varcChar = varcChar[-1]
#     }
#     for (i in start:end) {
#         arg$var = as.name(varChar[i])
#         dat = do.call(varSubset, arg)
#         l[i] = generateLine(type = type, mode = mode,
#                             low = low, high = high, col = col)
#         if (!is.null(col)) {
#             assign(paste0("df", i), dat)
#             rePattern = paste0("(^.+[)], colour = )col(, data = )dat(.+$)")
#             reReplace = paste0("\\1", "col[", i, "]", "\\2", "df", i, "\\3")
#             l[i] = gsub(rePattern, reReplace, l[i])
#         }
#     }
###############################################################################
