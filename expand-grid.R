library(RJSONIO)
library(magrittr)
setwd(file.path(normalizePath("~"),"kaggle-food"))
#f <- fromJSON("train.json")
f.full <- fromJSON("train.json", simplifyV=F)
data <- f.full#[1:1000]
system.time({
        data %<>% lapply(expand.grid) 
# %>% (function(x)x[[1]]) This was wrong, only first elements was used
# but anyway, expand.grid is slower than what I did it unlist variant
})
# 11 sec