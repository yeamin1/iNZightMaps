generateLine = function(type, mode = NULL, mode.p = NULL, mode.c = NULL,
                        geom = "polygon", grid = NULL, factor = FALSE,
                        low = NULL, low.p = NULL, low.c = NULL,
                        high = NULL, high.p = NULL, high.c = NULL,
                        size = NULL, size.p = NULL, size.c = NULL,
                        col = NULL, col.p = NULL, col.c = NULL,
                        cols = NULL, cols.p = NULL, cols.c = NULL,
                        solid = FALSE) {
    # Helper function to generate lines of command to draw maps.
    # Returns lines as strings which are parsed as text in drawPoint() and
    # drawContour().
    l = "(aes_string(x = \"lon\", y = \"lat\"), data = dat, na.rm = TRUE)"
    re = "(^.+aes_string[(].+)[)](,.*data = dat.+$)"
    
    ############
    ## POINTS ##
    ############
    if (type == "point" | type == "both") {
        pLine = paste0("geom_point", l)
        
        ## mode = "colour"
        if (any(grepl("colour", mode)) | any(grepl("colour", mode.p)))
            pLine = gsub(re, "\\1, colour = var)\\2", pLine)
        
        ## mode = "size"
        if (any(grepl("size", mode)) | (any(grepl("colour", mode.p)))) {
            pLine = gsub(re, "\\1, size = var)\\2", pLine)
            pLine = paste(pLine, "guides(size = guide_legend(\"size\"))",
                      sep = " + ")
        }
        
        ## mode = "alpha"
        if (any(grepl("alpha", mode)) | any(grepl("alpha", mode.p))) {
            pLine = gsub(re, "\\1, alpha = var)\\2", pLine)
            pLine = paste(pLine, "guides(alpha = guide_legend(\"alpha\"))",
                      sep = " + ")
        }
        
        ## mode = "shape"
        if (any(grepl("shape", mode)) | any(grepl("shape", mode.p))) {
            if (factor) {
                pLine = gsub(re, "\\1, shape = var)\\2", pLine)
                if (solid)
                    pLine = paste(pLine, "scale_shape(solid = TRUE)",
                                  sep = " + ")
            } else
                stop ("Variable must be a factor to use shape!")
        }
        
        ## low & high
        if (!is.null(low) & !is.null(high))
            pLine = paste(pLine, 
                          "scale_colour_gradient(low = low, high = high)",
                          sep = " + ")
        else if (!is.null(low.p) & !is.null(high.p))
            pLine = paste(pLine, 
                          "scale_colour_gradient(low = low.p, high = high.p)",
                          sep = " + ")
        
        ## col
        if (!is.null(col))
            pLine = gsub(re, "\\1), colour = col\\2", pLine)
        else if (!is.null(col.p))
            pLine = gsub(re, "\\1), colour = col.p\\2", pLine)
        
        ## size
        if (!is.null(size))
            pLine = gsub(re, "\\1), size = size\\2", pLine)
        else if (!is.null(size.p))
            pLine = gsub(re, "\\1), size = size.p\\2", pLine)
        
        ## grid
        if (!is.null(grid)) {
            if (factor) {
                if (length(grid) == 2)
                    pLine = paste(pLine, sep = " + ",
                                  paste("facet_wrap( ~ facetVariable,",
                                        "nrow = grid[1],", "ncol = grid[2])"))
                else
                    pLine = paste(pLine,
                                  paste0("facet_wrap( ~ facetVariable)"),
                                  sep = " + ")
            } else
                stop("Variable must be a factor to use grid!")
        }
        
        ## cols
        if (!is.null(cols)) {
            if (factor)
                pLine = paste(pLine, "scale_colour_manual(values = cols)",
                              sep = " + ")
            else
                stop ("Variable must be a factor to use cols!")
        } else if (!is.null(cols.p)) {
            if (factor)
                pLine = paste(pLine, "scale_colour_manual(values = cols.p)",
                              sep = " + ")
            else
                stop ("Variable must be a factor to use cols!")
        }
        
        ## type = "both"
        if (type == "both")
            invisible(pLine)
        else
            return(pLine)
    }
    
    #############
    ## CONTOUR ##
    #############
    if (type == "contour" | type == "both") {
        cLine = gsub(re, "stat_density2d\\1), bins = bins\\2", l)
        
        ## mode = "colour"
        if (any(grepl("colour", mode)) | any(grepl("colour", mode.c)))
            cLine = gsub(re, "\\1, colour = \"..level..\")\\2", cLine)
        
        ## mode = "alpha"
        if (any(grepl("alpha", mode)) | any(grepl("alpha", mode.c))) {
            cLine = gsub(re, "\\1, alpha = \"..level..\")\\2", cLine)
            cLine = paste(cLine, "guides(alpha = guide_legend(\"alpha\"))",
                          sep = " + ")
        }
        
        ## size
        if (!is.null(size))
            cLine = gsub(re, "\\1), size = size\\2", cLine)
        else if (!is.null(size.c))
            cLine = gsub(re, "\\1), size = size.c\\2", cLine)
        
        ## geom = "polygon"
        if (grepl("polygon", geom)) {
            cLine = gsub(re, "\\1), geom = \"polygon\"\\2", cLine)
            
            ## mode = "fill"
            if (any(grepl("fill", mode)) | any(grepl("fill", mode.c))) {
                cLine = gsub(re, "\\1, fill = \"..level..\")\\2", cLine)
                cLine = paste(cLine, "guides(fill = guide_legend(\"fill\"))",
                              sep = " + ")
            }
            
            ## low & high
            if (!is.null(low) & !is.null(high))
                cLine = paste(cLine, 
                              paste0("scale_fill_gradient",
                                     "(low = low, high = high)"),
                              sep = " + ")
            else if (!is.null(low.c) & !is.null(high.c))
                cLine = paste(cLine, 
                              paste0("scale_fill_gradient",
                                     "(low = low.c, high = high.c)"),
                              sep = " + ")
            
        ## geom = "density2d"
        } else if (grepl("density2d", geom)) {
            cLine = gsub(re, "\\1), geom = \"density2d\"\\2", cLine)
            
            ## low & high
            if ((!is.null(low) & !is.null(high)) | 
                    (!is.null(low.c) & !is.null(high.c)) -> both) {
                if (all(!grepl("colour", mode)) | all(!grepl("colour", mode)))
                    cLine = gsub(re, "\\1, colour = \"..level..\")\\2", cLine)
                if (both)
                    l = paste0("scale_colour_gradient",
                               "(low = low.c, high = high.c)")
                else
                    l = paste0("scale_colour_gradient",
                               "(low = low, high = high)")
                cLine = paste(cLine, l, sep = " + ")
            }
        }
        
        ## col
        if (!is.null(col))
            cLine = gsub(re, "\\1), colour = col\\2", cLine)
        else if (!is.null(col.c))
            cLine = gsub(re, "\\1), colour = col.c\\2", cLine)
        
        ## size
        if (!is.null(size) | !is.null(size.c))
            cLine = gsub(re, "\\1), size = size\\2", cLine)
        else if (!is.null(size.c))
            cLine = gsub(re, "\\1), size = size.c\\2", cLine)
        
        ## grid
        if (!is.null(grid)) {
            if (factor) {
                if (length(grid) == 2)
                    cLine = paste(cLine, sep = " + ",
                                  paste("facet_wrap( ~ facetVariable,",
                                        "nrow = grid[1],", "ncol = grid[2])"))
                else
                    cLine = paste(cLine,
                                  paste0("facet_wrap( ~ facetVariable)"),
                                  sep = " + ")
            } else
                stop("Variable must be a factor to use grid!")
        }
        
        ## cols
        if (!is.null(cols)) {
            if (factor) {
                cLine = paste(cLine, "scale_colour_manual(values = cols)",
                              sep = " + ")
            } else
                stop ("Variable must be a factor to use cols!")
        } else if (!is.null(cols.c)) {
            if (factor) {
                cLine = paste(cLine, "scale_colour_manual(values = cols.c)",
                              sep = " + ")
            } else
                stop ("Variable must be a factor to use cols!")
        }
        
        ## type = "both
        if (type == "both") 
            invisible(cLine)
        else
            return(cLine)
    }
    
    ##########
    ## BOTH ##
    ##########
    if (type == "both")
        return(paste(pLine, cLine, sep = " + "))
    
    ##############
    ## no match ##
    ##############
    if (type != "point" | type != "contour" | type != "both")
        stop("Type must be either \"point\", \"contour\ or \"both\".")
    
}
