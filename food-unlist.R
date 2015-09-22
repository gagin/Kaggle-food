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
        s1 <- strsplit(x=s, split="oz.) ", fixed=TRUE)[[1]][2]
        if(is.na(s1)) s else s1
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
})
colnames(data) <- c("id","cuisine","ingridient")
# This matrix approach converts full data set just in 3 seconds

system.time({
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

probs.cut<-ifelse(probs<0.5,0,probs)

})

debug <- FALSE
blank <- rep(0,length(cuisines))
# cell <- data.frame(1) # not needed anymore - tried to use to fix colnames
tick <- progress()
for(i in 1:length(test.ids)) {
        tick()
        cu.scores <- blank
        for(ing in test.list[[i]][[2]])
                if(ing %in% ings.tr) {
                        if(debug) cat(paste0(ing,"\n"))
                        cu.scores <- cu.scores + probs.cut[ing,]^2
                }
        res[i,2]<-names(cu.scores)[which.max(cu.scores)]
}


write.csv(res,"submit2cut.5.csv",row.names=FALSE, quote=FALSE)


                