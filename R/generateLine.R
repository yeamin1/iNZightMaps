generateLine = function(type, mode = NULL, geom,
                        low = NULL, high = NULL, col = NULL, cols = NULL,
                        size = NULL, factor = FALSE, grid = NULL) {
    # Helper function to generate lines of command to draw maps.
    # Returns lines as strings which are parsed as text in drawPoint() and
    # drawContour().
    l = "(aes_string(x = \"lon\", y = \"lat\"), data = dat, na.rm = TRUE)"
    re = "(^.+aes_string[(].+)[)](,.*data = dat.+$)"
    
    if (type == "point" | type == "both") {
        pLine = paste0("geom_point", l)
        if (any(grepl("colour", mode))) {
            pLine = gsub(re, "\\1, colour = var)\\2", pLine)
        }
        if (any(grepl("size", mode))) {
            pLine = gsub(re, "\\1, size = var)\\2", pLine)
            pLine = paste(pLine, "guides(size = guide_legend(\"size\"))",
                      sep = " + ")
        }
        if (any(grepl("alpha", mode))) {
            pLine = gsub(re, "\\1, alpha = var)\\2", pLine)
            pLine = paste(pLine, "guides(alpha = guide_legend(\"alpha\"))",
                      sep = " + ")
        }
        if (any(grepl("shape", mode))) {
            if (factor)
                pLine = gsub(re, "\\1, shape = var)\\2", pLine)
            else
                stop ("Variable must be a factor to use shape!")
        }
        if (!is.null(low) & !is.null(high))
            pLine = paste(pLine, 
                          "scale_colour_gradient(low = low, high = high)",
                          sep = " + ")
        
        if (!is.null(col))
            pLine = gsub(re, "\\1), colour = col\\2", pLine)
        if (!is.null(size))
            pLine = gsub(re, "\\1), size = size\\2", pLine)
        if (!is.null(grid)) {
            if (factor) {
                if (length(grid) == 2)
                    pLine = paste(pLine, sep = " + ",
                                  paste("facet_wrap( ~ arg$var,", "nrow = grid[1],", 
                                        "ncol = grid[2])"))
                else
                    pLine = paste(pLine,
                                  paste0("facet_wrap( ~ facetVariable)"),
                                  sep = " + ")
            } else
                stop("Variable must be a factor to use grid!")
        }
        if (!is.null(cols)) {
            if (factor) {
                pLine = paste(pLine, "scale_colour_manual(values = cols)",
                              sep = " + ")
            } else
                stop ("Variable must be a factor to use cols!")
        }
        
        if (type == "both")
            invisible(pLine)
        else
            return(pLine)
    }
    
    if (type == "contour" | type == "both") {
        cLine = gsub(re, "stat_density2d\\1), bins = bins\\2", l)
        if (grepl("polygon", geom)) {
            cLine = gsub(re, "\\1), geom = \"polygon\"\\2", cLine)
            if (any(grepl("colour", mode)))
                cLine = gsub(re, "\\1, colour = \"..level\")\\2", cLine)
            if (any(grepl("fill", mode))) {
                cLine = gsub(re, "\\1, fill = \"..level..\")\\2", cLine)
                cLine = paste(cLine, "guides(fill = guide_legend(\"fill\"))",
                              sep = " + ")
            }
            if (any(grepl("alpha", mode))) {
                cLine = gsub(re, "\\1, alpha = \"..level..\")\\2", cLine)
                cLine = paste(cLine, "guides(alpha = guide_legend(\"alpha\"))",
                              sep = " + ")
            }
            if (!is.null(low) & !is.null(high))
                cLine = paste(cLine, "scale_fill_gradient(low = low, high = high)",
                          sep = " + ")
        } else if (grepl("density2d", geom)) {
            cLine = gsub(re, "\\1), geom = \"density2d\"\\2", cLine)
            if (!is.null(size))
                cLine = gsub(re, "\\1), size = size\\2", cLine)
        }
        
        if (!is.null(col)) {
            cLine = gsub(re, "\\1), colour = col\\2", cLine)
        }
        
        if (type == "both") 
            invisible(cLine)
        else
            return(cLine)
    }
        
    if (type == "both")
        paste(pLine, cLine, sep = " + ")
    
    if (type != "point" | type != "contour" | type != "both")
        stop("Type must be either \"point\", \"contour\ or \"both\".")
    
}
