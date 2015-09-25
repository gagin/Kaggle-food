answers <- data.table(id=sapply(f, "[[", 1), cuisine=sapply(f, "[[", 2))
setkey(answers,id) # It messes up the order
testers <- data.table(id=sapply(f, "[[", 1), ingredients=sapply(f, "[[", 3))
testers[, ingredients := sapply(ingredients, sapply, cleanup)]
setkey(testers,id)
        
# 1000 to make it definitive marker when it only mentioned for a single cuisine
probs.cut <- ifelse(probs==1,1000,probs^3)#ifelse(probs<0.2, 0, probs)
probsDT <- data.table(probs.cut)[, ingredient := rownames(probs.cut)]
setkey(probsDT, ingredient)

sample.size <- 20
results <- integer()
tries <- 20
for(i in 1:tries) {
set.seed(i)
picks <- as.numeric(sample(unique(data[, 1]), sample.size))
found <- sapply(picks,
       function(x)
               CuisineByIngredients(testers[J(x), 2, with=FALSE][[1]][[1]]))
actual<-answers[J(as.numeric(picks))]$cuisine
results[i] <- sum(found==actual)
}
summary(results)

### Debug case 1
# seed 20
# ifelse(probs==1,1000,(probs-0.1)^3)
cat(found)
cat(actual)
# Irish considered British
picks[20]
answers[J(37055)]
testers[J(37055)]$ingredients
#probsDT[J("banger")]
probs["banger",]
row.names(counts) <- ings.tr
counts["banger",]
probs["leeks",]
counts["leeks",]
CuisineByIngredients(testers[J(37055)]$ingredients[[1]])
# So, banger in both and little bit more British. Leeks is twice as Irish,
# but is also in many others, so weight isn't as strong then.
# So, project zeros to other ingrs first, then do probs?

# "0" and as.numeric added because of apply() convertion to character
ZeroIfZero <- function(x) if(sum(ifelse(x=="0",1L,0L))>0L) as.numeric(x)*0 else x

# Don't forget to drop summing there instead
countsDT <- data.table(counts[,-ncol(counts)])[, ingredient := ings.tr]
setkey(countsDT, ingredient)

CuisineByIngredients2 <- function(ings) {
        step1 <- countsDT[J(ings), nomatch=0]
        # next step breaks because it converts everything to character
        step2 <- apply(step1, 2, ZeroIfZero)
        step2a <- data.table(step2)
        step2a[,sums:=sum(as.numeric(.SD)),by=ingredient]
        # skip check if just one ingridient, which will break colCums
        #step3 <- if(is.null(dim(bas))) bas <- colSums(bas)
        step4 <- colSums(step2)
        step5 <- which.max(step4)
        cuisines[step5]
}

b<-probsDT[J(c("banger","leeks")),-21,with=FALSE]
