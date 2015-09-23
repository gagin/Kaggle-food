library(RJSONIO)
library(magrittr)
library(data.table)
library(tidyr)
source("https://raw.githubusercontent.com/gagin/R-tricks/master/progress.R")

setwd(file.path(normalizePath("~"),"kaggle-food"))
#f <- fromJSON("train.json")
system.time({
        f.full <- fromJSON("train.json", simplifyV=F)
})
        f <- f.full#[1:1000]

cleanup <- function(s) {
        s <- tolower(s)
        s <- gsub('&', '', s, fixed=TRUE)
        # remove oz.) prefixes
        if(
                length(
                        s<-strsplit(x=s,
                                    split="oz.) ",
                                    fixed=TRUE
                                    )[[1]]
                        ) > 1
        ) s<- s[2]
        # remove lb. prefixes
        if(
                length(
                        s<-strsplit(x=s,
                                    split="lb. ",
                                    fixed=TRUE
                        )[[1]]
                ) > 1
        ) s<- s[2]
        
        s
}


system.time({
        data <- f %>%
        lapply(
                function(x)
                        lapply(x$ingredients,
                               function(y)
                                       list(id=x$id,
                                            cuisine=x$cuisine,
                                            ingredient=cleanup(y)
                                       )
                        )
        ) %>%                 
        unlist %>%
        matrix(ncol=3, byrow=TRUE)

colnames(data) <- c("id","cuisine","ingridient")
# This matrix approach converts full data set in just 3 seconds,
# 7 if to clean up "(N oz.)" prefixes right away


        counts <- table(as.data.table(data)[,.(cuisine,ingridient)]) %>%
                as.data.frame %>%
                spread(cuisine, Freq)
        ings.tr <- counts$ingridient
        counts <- counts[,-1]
        cuisines <- colnames(counts)
        counts$sum <- rowSums(counts)
        
        probs <- sapply(counts,function(x) x/counts$sum)
        row.names(probs) <- ings.tr
        probs<-probs[, -ncol(probs)]
})

system.time({
        test.list <- fromJSON("test.json", simplifyV=F)
})

probs.cut<-probs #ifelse(probs<0.2,0,probs)


CuisineByIngredients <- function(ings) {
        if(length(ings)==0) "wrong" else
        cuisines[
                ings %>%
                #sapply(cleanup) %>%
                lapply(function(y) probs.cut[y,]) %>%
                unlist %>%
                matrix(ncol=20, byrow=TRUE) %>%
                apply(2,sum) %>%
                which.max
                ]
}

tick <- progress()
system.time({
res <- lapply(test.list,
             function(x) {
                     tick()
                     raw.ings <- x[[2]]
                     clean.ings <- sapply(raw.ings, cleanup)
                     known.ings <- clean.ings[clean.ings %in% ings.tr]
                     c(x[[1]],
                       CuisineByIngredients(known.ings))}
             ) %>% unlist %>% matrix(ncol=2, byrow=TRUE)
})

colnames(res) <- c("id","cuisine")
write.csv(res,"submit-apply-nocut-linear.csv",row.names=FALSE, quote=FALSE)


                