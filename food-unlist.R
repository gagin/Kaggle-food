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

system.time({
        
ings.test <- unique(unlist(sapply(test.list,function(x) unique(x[[2]]))))

# Do we have any left?
# ings.test[!ings.test %in% ings.tr]

test.ids <- sapply(test.list,function(x)x[[1]])

res <- data.frame(matrix(0, nrow=length(test.ids), ncol=2))
colnames(res) <- c("id", "cuisine")
res$cuisine <- as.character(res$cuisine)
res$id <- test.ids

probs.cut<-probs #ifelse(probs<0.2,0,probs)
})


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
re <- lapply(test.list,
             function(x) {
                     tick()
                     raw.ings <- x[[2]]
                     clean.ings <- sapply(raw.ings, cleanup)
                     known.ings <- clean.ings[clean.ings %in% ings.tr]
                     c(x[[1]],
                       CuisineByIngredients(known.ings))}
             ) %>% unlist %>% matrix(ncol=2, byrow=TRUE)
})

system.time({
blank <- rep(0,length(cuisines))
tick <- progress()
for(i in 1:length(test.ids)) {
        tick()
        cu.scores <- blank
        for(ing in test.list[[i]][[2]]) {
                ing <- cleanup(ing)
                if(ing %in% ings.tr)
                        cu.scores <- cu.scores + probs.cut[ing,]
        }
        res[i,2]<-names(cu.scores)[which.max(cu.scores)]
}
})

write.csv(res,"submit2cut.5.csv",row.names=FALSE, quote=FALSE)


                