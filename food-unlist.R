library(RJSONIO)
library(dplyr)
library(data.table)
setwd(file.path(normalizePath("~"),"kaggle-food"))
#f <- fromJSON("train.json")
f <- fromJSON("train.json", simplifyV=F)
f1 <- f[1:10000]
system.time(
        data <- f1 %>%
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
        unlist(recur = FALSE) %>%
        lapply(unlist) %>%
        as.data.table %>%
        t
)

# On 10000 records 36.77 sec, while direct for loop with df assigns 57.55
# Which is strange as with 1k record for loop was faster

# with data.table it's just 6.33!
# Full data set is just 27.34!

# Perhaps next step in building counts will be slower here though?

# But cell assignment to data.table[1,1] was very slow - because it's a wrong
# syntax actually, it should be data.table[1]$columnname