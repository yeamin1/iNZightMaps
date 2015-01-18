timeCat = function(currentTime, completeTime) {
    cat(paste0("done!", " (",
               round(completeTime - currentTime, digits = 2),
               " sec)", "\n"))
}
