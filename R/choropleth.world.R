# user = read.csv("//Users/eric/Desktop/userlocation.csv")

choropleth.world = function(data, region, value = NULL, title = NULL,
                            low = "#DDFFDD", high = "#005500") {
    
    suppressPackageStartupMessages(library(ggplot2))
    
    world = map_data(map = "world")
    world[world$region == "USSR", "region"] = "Russia"
    world[world$region == "UK", "reigon"] = "United Kingdom"
    world[world$region == "USA", "region"] = "United States"
    
    world$region = tolower(world$region)
    
    region = deparse(substitute(region))
    data = data[!is.na(data[, region]) & data[, region] != "", ] # missing data
    data[, region] = tolower(data[, region])
    if (is.factor(data[, region]))
        data[, region] = as.character(data[, region])
    if (any(duplicated(data[, region]))) {
        data = as.data.frame(table(as.character(data[, region])))
        colnames(data)[1] = region
    }
    
    if (!is.null(value)) 
        fill = deparse(substitute(vaule))
    else 
        fill = "Freq"
    
    ggplot() + geom_map(aes_string(map_id = region, fill = fill),
                        map = world, data = data) + 
        expand_limits(x = world$long, y = world$lat) + 
        geom_polygon(data = world, aes(x = long, y = lat, group = group), 
                     colour = "black", fill = NA)  + 
        coord_equal() + labs(title = title, x = NULL, y = NULL) +
        scale_fill_gradient(low = low, high = high)
}
