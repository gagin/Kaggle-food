library(dplyr)

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

counter <- 0
for(i in ings.tr) {      
        counter <- counter + 1
        if(counter == 100) {
                cat(paste("Processing row",r,"\n"))
                counter <- 0
        }
        train[i] <- ifelse(is.na(train[i]),0,1)
}
# 2Gb
counter <- 0
for(i in ings.tr) {      
        counter <- counter + 1
        if(counter == 100) {
                cat(paste("Processing row",r,"\n"))
                counter <- 0
        }
        train[i] <- sapply(train[i],as.integer)
}
# 1Gb again

# lapply doesn't give performance improvement anyway, need some vectorized func

cuisines <- unique(train$cuisine)
for(i in ings.tr) {      
        counts[[i]]<-table(train[train[i] == 1, "cuisine"])
}

props <- data.frame(matrix(0,nrow=length(ings),ncol=length(cuisines)))
rownames(props)<-ings
colnames(props)<-cuisines

for(i in ings.tr)
        for(cu in cuisines)
                props[i,cu]<-counts[[i]][[cu]]

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

blank <- rep(0,length(cuisines))
cell <- data.frame(1)
for(i in 1:length(test.ids)) {
        cu.scores <- blank
        for(ing in test.list[[i]][[2]])
                if(ing %in% ings.train) {
                        ing
                        cat(ing)
                        cu.scores <- cu.scores + probs[ing,]
                }
        res[i,2]<-names(cu.scores)[which.max(cu.scores)]
}
        

write.csv(res,"submit.csv",row.names=FALSE, quote=FALSE)
