draw = function(data, var, var.cond = NULL,
                location, zoom = NULL, maptype = "terrain", src = "google",
                # animate = FALSE, n = 5, #
                type, mode, geom = NULL, bins = NULL, grid = NULL,
                low = NULL, high = NULL, solid = FALSE,
                size = NULL, title = NULL, col = NULL, cols = NULL, 
                mode.p = NULL, size.p = NULL, col.p = NULL,
                mode.c = NULL, size.c = NULL, col.c = NULL,
                low.p = NULL, high.p = NULL, cols.p = NULL,
                low.c = NULL, high.c = NULL, cols.c = NULL) {
    
    loc = getBB(location)
    baseMap = drawMap(loc, zoom, maptype, src)
    pars = as.list(match.call())[-1]
    if (type == "point") {
        argString = c("data", "var", "var.cond", "type", "mode", "solid",
                      "low", "high", "col", "cols", "size", "title", "grid")
        f = "drawPoints"
    } else if (type == "contour") {
        argString = c("data", "var", "var.cond", "type", "mode","solid",
                      "geom", "bins", "low", "high", "col", "cols", 
                      "size", "title", "grid")
        f = "drawContour"
    } else if (type == "both") {
        argString = c("data", "var", "var.cond", "type", "geom", "bins",
                      "mode.p", "mode.c", "size.p", "size.c",
                      "low.p", "low.c","high.p", "high.c",
                      "col.p", "col.c", "cols.p", "cols.c",
                      "title", "grid")
        f = "drawBoth"
    }
    
    arg = pars[which(names(pars) %in% argString)]
    arg$loc = loc
    arg$baseMap = baseMap
    map = do.call(f, arg)
    return(map)
}
