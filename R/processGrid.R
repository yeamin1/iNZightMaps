processGrid = function(grid) {
    if (isNrow(grid) & !isNcol(grid)) {
        if (length(grid) == 2)
            grid = c(grid[getNrow(grid)], grid[!getNrow(grid)])
        else
            grid = c(grid[getNrow(grid)], NULL)
    } else if (!isNrow(grid) & isNcol(grid)) {
        if (length(grid) == 2) 
            grid = c(grid[!getNcol(grid)], grid[getNcol(grid)])
        else
            grid = c(NULL, grid[getNcol(grid)])
    } else if (isNrow(grid) & isNcol(grid))
        grid = c(grid[getNrow(grid)], grid[getNcol(grid)])
    return(grid)
}

# GETTERS for nrow & ncol
getNrow = function(grid) { names(grid) == "nrow" }
getNcol = function(grid) { names(grid) == "ncol" }

# Logical
isNrow = function(grid) { sum(getNrow(grid)) == 1 }
isNcol = function(grid) { sum(getNcol(grid)) == 1 }
