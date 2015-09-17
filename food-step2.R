library(dplyr)

source("https://raw.githubusercontent.com/gagin/R-tricks/master/progress.R")

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

tick <- progress()
for(i in ings.tr) {      
        tick(i)
        train[i] <- ifelse(is.na(train[i]),0,1)
}
# 2Gb
#tick <- progress()
#for(i in ings.tr) {      
#        tick(i)
#        train[i] <- sapply(train[i],as.integer)
#}
# 1Gb again

# lapply doesn't give performance improvement anyway, need some vectorized func

cuisines <- unique(train$cuisine)
counts <- list()
tick <- progress()
for(i in ings.tr) {
        tick(i)
        counts[[i]]<-table(train[train[i] == 1, "cuisine"])
}

props <- data.frame(matrix(0,nrow=length(ings.tr),ncol=length(cuisines)))
rownames(props)<-ings.tr
colnames(props)<-cuisines

tick <- progress()
for(i in ings.tr) {
        tick(i)
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

probs.cut<-ifelse(probs<0.5,0,probs)

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
