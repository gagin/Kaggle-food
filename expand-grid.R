library(RJSONIO)
library(dplyr)
library(data.table)
setwd(file.path(normalizePath("~"),"kaggle-food"))
#f <- fromJSON("train.json")
f.full <- fromJSON("train.json", simplifyV=F)
f <- f.full#[1:1000]
atta.constr <- function(x) {
        full <- data.frame(matrix(nrow=0, ncol=3))
        add <- function(x) {        
        full <<- rbind(full,expand.grid(x))
        }
        get <- function() return(full)
        list(add=add, get=get)
}
atta <- atta.constr()

system.time({
blowup <- lapply(f, atta$add)
full <- atta$get()
})
