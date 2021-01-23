# Title     : TODO
# Objective : TODO
# Created by: Konsti
# Created on: 23.01.2021

str_precision = function(numvec){
    #' vectorise with purrr
    vec = map_dbl(numvec, function(numeric){
        #cat(numeric)
        char = as.character(abs(numeric))
        split = str_split_fixed(char, pattern = "\\.", n = 2)[2]
        #cat(split)
        return(str_length(split))
    })
    return(vec)
}
