library(RJSONIO)
setwd(file.path(normalizePath("~"),"kaggle-food"))
#f <- fromJSON("train.json")
f <- fromJSON("train.json", simplifyV=F)
train <- data.frame(id=sapply(f,function(x)x[[1]]),
                cuisine=sapply(f,function(x)x[[2]]))
ings.train <- unique(unlist(sapply(f,function(x) unique(x[[3]]))))
#train2 <- data.frame(id=sapply(f,function(x)x[[1]]),
#                     cuisine=sapply(f,function(x)x[[2]]),
#                     ings)
counter <- 0
for(r in 1:length(f)) {
        counter <- counter + 1
        if(counter == 100) {
                cat(paste("Processing row",r,"\n"))
                counter <- 0
        }
        for(i in 1:length(f[[r]][[3]]))
                train[r,f[[r]][[3]][[i]]]<-1
}
write.csv(train,"train.csv")

f <- fromJSON("test.json", simplifyV=F)
test <- data.frame()#,
              #  cuisine=sapply(f,function(x)x[[2]]))
counter <- 0
for(r in 1:length(f)) {
        counter <- counter + 1
        if(counter == 100) {
                cat(paste("Processing row",r,"\n"))
                counter <- 0
        }
        for(i in 1:length(f[[r]][[2]]))
                test[r,f[[r]][[2]][[i]]]<-1
}
write.csv(test,"test.csv")

