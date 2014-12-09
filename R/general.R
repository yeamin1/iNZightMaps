timeCat = function(currentTime, completeTime) {
    cat(paste0("done!", " (",
               signif(completeTime - currentTime, digits = 3), 
               " sec)", "\n"))
}
