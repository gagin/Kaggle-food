library(RJSONIO)
library(dplyr)
library(data.table)
setwd(file.path(normalizePath("~"),"kaggle-food"))
#f <- fromJSON("train.json")
f.full <- fromJSON("train.json", simplifyV=F)
f <- f.full#[1:1000]
system.time({
        data <- f %>%
        lapply(
                function(x)
                        lapply(x$ingredients,
                               function(y)
                                       list(id=x$id,
                                            cuisine=x$cuisine,
                                            ingredient=y
                                       )
                        )
        ) %>%                 
        unlist %>%
        matrix(ncol=3, byrow=TRUE)
})

# This matrix approach converts full data set just in 3 seconds

# Perhaps next step in building counts will be slower here though?