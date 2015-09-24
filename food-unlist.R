library(RJSONIO)
library(magrittr)
library(data.table)
library(tidyr)
source("https://raw.githubusercontent.com/gagin/R-tricks/master/progress.R")

setwd(file.path(normalizePath("~"),"kaggle-food"))
f.full <- fromJSON("train.json", simplifyV=F)
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
        # remove anything after comma
        if(
                length(
                        s<-strsplit(x=s,
                                    split=",",
                                    fixed=TRUE
                        )[[1]]
                ) > 1
        ) s<- s[1]
        s
}

# convert json-resulted list to matrix
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
colnames(data) <- c("id", "cuisine", "ingridient")

counts <- table(as.data.table(data)[, .(cuisine,ingridient)]) %>%
        as.data.frame %>%
        spread(cuisine, Freq)
ings.tr <- counts$ingridient
counts <- counts[,-1]
cuisines <- colnames(counts)
counts$sum <- rowSums(counts)

# replace counts with shares of cases
probs <- sapply(counts, function(x) x/counts$sum)
row.names(probs) <- ings.tr
probs <- probs[, -ncol(probs)]

test.list <- fromJSON("test.json", simplifyV=F)

probs.cut <- (probs-0.1)^3#ifelse(probs<0.2, 0, probs)
probsDT <- data.table(probs.cut)[, ingredient := rownames(probs.cut)]
setkey(probsDT, ingredient)
last.column <- ncol(probsDT)

# This version is safer but slower, and there's seem no completely empty ones
#CuisineByIngredientsDT <- function(ings) {
#        cross <- probsDT[J(ings), -ncol(probsDT), with=FALSE]
#        if(nrow(cross)==0)
#                "wrong"
#        else
#                cuisines[which.max(colSums(cross, na.rm=TRUE))]
#}

CuisineByIngredients <- function(ings)
        cuisines[which.max(colSums(probsDT[J(ings),
                                           -last.column,
                                           with=FALSE],
                                   na.rm=TRUE))]

res <- data.table(id=sapply(test.list, "[[", 1))
res$cuisine <- sapply(
        sapply(lapply(test.list,"[[", 2),
               sapply,
               cleanup),
        CuisineByIngredients)

write.csv(res, "submit6-drop-after-comma.csv", row.names=FALSE, quote=FALSE)