library(dplyr)

progress <- function(add="", reset=FALSE, steps=100, msg="Processing row") {
# Potentially conflicts with httr::progress
        if(reset) {
                progress.counter <<- 0
                progress.hundreds <<- 0
        }
        progress.counter <<- progress.counter + 1
        if(progress.counter == steps) {
                progress.hundreds <<- progress.hundreds + 1
                if(add != "") add <- paste(":",add)
                cat(paste0(msg,
                           " ",
                           progress.hundreds*steps,
                           add,
                           "\n"))
                progress.counter <<- 0
        }
}

setwd(file.path(normalizePath("~"),"kaggle-food"))

train <- read.csv("train.csv", check.names=FALSE)
#library(pryr); object_size(train)
train.backup <- train
ings.tr <- names(train)[-c(1,2,3)]
#train[ings] <- sapply(train[ings], function(x) ifelse(is.na(x),0,1))
#Error: cannot allocate vector of size 2.0 Gb

#ings.train.dots<-gsub(" ",".",ings.train,fixed=TRUE)
#ings.train.dots[! ings.train.dots %in% names(train)[-c(1,2,3)]]
# Still sucks

progress(reset=TRUE)
for(i in ings.tr) {      
        progress(i)
        train[i] <- ifelse(is.na(train[i]),0,1)
}
# 2Gb
#progress(reset=TRUE)
#for(i in ings.tr) {      
#        progress(i)
#        train[i] <- sapply(train[i],as.integer)
#}
# 1Gb again

# lapply doesn't give performance improvement anyway, need some vectorized func

cuisines <- unique(train$cuisine)
counts <- list()
progress(reset=TRUE)
for(i in ings.tr) {
        progress(i)
        counts[[i]]<-table(train[train[i] == 1, "cuisine"])
}

props <- data.frame(matrix(0,nrow=length(ings.tr),ncol=length(cuisines)))
rownames(props)<-ings.tr
colnames(props)<-cuisines

progress(reset=TRUE)
for(i in ings.tr) {
        progress(i)
        for(cu in cuisines)
                props[i,cu]<-counts[[i]][[cu]]
}

props$sum<-rowSums(props)
probs <- sapply(props,function(x) x/props$sum)
rownames(probs) <- rownames(props)
probs<-probs[, -ncol(probs)]

test.list <- fromJSON("test.json", simplifyV=F)

ings.test <- unique(unlist(sapply(test.list,function(x) unique(x[[2]]))))
#ings.test[!ings.test %in% ings.train]
ings.test[!ings.test %in% ings.tr]

test.ids <- sapply(test.list,function(x)x[[1]])

res <- data.frame(matrix(0, nrow=length(test.ids), ncol=2))
colnames(res) <- c("id", "cuisine")
res$cuisine <- as.character(res$cuisine)
res$id <- test.ids

debug <- FALSE
blank <- rep(0,length(cuisines))
# cell <- data.frame(1) # not needed anymore - tried to use to fix colnames
progress(reset=TRUE)
for(i in 1:length(test.ids)) {
        progress()
        cu.scores <- blank
        for(ing in test.list[[i]][[2]])
                if(ing %in% ings.tr) {
                        if(debug) cat(paste0(ing,"\n"))
                        cu.scores <- cu.scores + probs[ing,]^2
                }
        res[i,2]<-names(cu.scores)[which.max(cu.scores)]
}
        

write.csv(res,"submit.csv",row.names=FALSE, quote=FALSE)
