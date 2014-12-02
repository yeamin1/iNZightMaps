processFactor = function (argVar, l, mode) {
    replace = paste0("= \"factor(", argVar, ")\"")
    l = gsub("= var", replace, l)
    if (any(grepl("shape", mode)))
        l = paste(l, "scale_shape_manual(values = 1:n)", sep = " + ")
    if (grepl("^.+facet_wrap[(] ~ facetVariable.+$", l))
        l = gsub("facetVariable", argVar, l)
    return(l)
}

isFactor = function(argVar) {
    ret = FALSE
    if (length(argVar) == 2 & grepl("^factor$", argVar[[1]]))
        ret = TRUE
    return(ret)
}
