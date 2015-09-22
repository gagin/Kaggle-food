library(RJSONIO)
library(magrittr)
setwd(file.path(normalizePath("~"),"kaggle-food"))
#f <- fromJSON("train.json")
f.full <- fromJSON("train.json", simplifyV=F)
data <- f.full#[1:1000]
system.time({
        data %<>% lapply(expand.grid) %>% (function(x)x[[1]])
})
# 11 sec