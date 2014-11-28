generateLine = function(type, mode, geom,
                        low = NULL, high = NULL, range = NULL, size = NULL) {
    pars = as.list(match.call())
    l = "(aes_string(x = \"lon\", y = \"lat\"), data = dat, na.rm = TRUE)"
    re = "(^.+aes_string[(].+)[)](,.*data = dat.+$)"
    
    if (type == "point" | type == "both") {
        pLine = paste0("geom_point", l)
        if (any(grepl("colour", mode))) {
            pLine = gsub(re, "\\1 , colour = var)\\2", pLine)
            pLine = paste(pLine, "guides(colour = guide_legend(\"colour\"))",
                      sep = " + ")
        }
        if (any(grepl("size", mode))) {
            pLine = gsub(re, "\\1 , size = var)\\2", pLine)
            pLine = paste(pLine, "guides(size = guide_legend(\"size\"))",
                      sep = " + ")
        }
        if (any(grepl("alpha", mode))) {
            pLine = gsub(re, "\\1, alpha = var)\\2", pLine)
            pLine = paste(pLine, "guides(alpha = guide_legend(\"alpha\"))",
                      sep = " + ")
        }
        if (length(low) != 0 & length(high) != 0)
            pLine = paste(pLine, "scale_fill_gradient(low = low, high = high)",
                          sep = " + ")
        if (type == "both")
            invisible(pLine)
        else
            return(pLine)
    }
    
    if (type == "contour" | type == "both") {
        cLine = gsub(re, "stat_density2d\\1), bins = bins\\2", l)
        if (grepl("polygon", geom)) {
            cLine = gsub(re, "\\1), geom = \"polygon\"\\2", cLine)
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
            if (length(low) != 0 & length(high) != 0)
                cLine = paste(cLine, "scale_fill_gradient(low = low, high = high)",
                          sep = " + ")
            if (length(range) != 0)
                cLine = paste(cLine, "scale_alpha(range = range)", sep = " + ")
        } else if (grepl("density2d", geom)) {
            cLine = gsub(re, "\\1), geom = \"density2d\"\\2", cLine)
            if (length(size) != 0)
                cLine = gsub(re, "\\1), size = size\\2", cLine)
        }
        if (type == "both") 
            invisible(cLine)
        else
            return(cLine)
    }
    
    if (type == "both")
        paste(pLine, cLine, sep = " + ")
}
