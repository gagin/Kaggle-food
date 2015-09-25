### Common
answers <- data.table(id=sapply(f, "[[", 1), cuisine=sapply(f, "[[", 2))
setkey(answers,id) # It messes up the order
testers <- data.table(id=sapply(f, "[[", 1), ingredients=sapply(f, "[[", 3))
testers[, ingredients := sapply(ingredients, sapply, cleanup)]
setkey(testers,id)
        
### Algorithm 1
# 1000 to make it definitive marker when it only mentioned for a single cuisine
probs.cut <- ifelse(probs==1,1000,probs^3)#ifelse(probs<0.2, 0, probs)
probsDT <- data.table(probs.cut)[, ingredient := rownames(probs.cut)]
setkey(probsDT, ingredient)

### Algorithm 2
ZeroIfZero <- function(x) if(sum(ifelse(x==0,1L,0L))>0L) x*0L else x
countsDT <- data.table(counts[,-ncol(counts)])[, ingredient := ings.tr]
setkey(countsDT, ingredient)
CuisineByIngredients2 <- function(ings) {
        step1 <- countsDT[J(ings), -21, with=FALSE, nomatch=0]
        step2 <- apply(step1, 2, ZeroIfZero)
        step2DT <- data.table(step2)
        # Thanks to http://stackoverflow.com/questions/32617619
        # There also was "* (temp > 0)" to account for zero sums, but
        # I don't have it here, so for performance it can be dropped
        step2DT[, names(step2DT) := {temp = rowSums(.SD); (.SD / temp)}]
        # skip check if just one ingridient, which will break colCums
        #step3 <- if(is.null(dim(bas))) bas <- colSums(bas)
        step4 <- colSums(step2DT)
        step5 <- which.max(step4)
        cuisines[step5]
}

### Case 1 - Algorithm 1
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

### Case 2 - Algorithm 2
sample.size <- 100
results <- integer()
tries <- 100
for(i in 1:tries) {
        set.seed(i)
        picks <- as.numeric(sample(unique(data[, 1]), sample.size))
        found <- sapply(picks,
                        function(x)
                                CuisineByIngredients2(testers[J(x), 2, with=FALSE][[1]][[1]]))
        actual<-answers[J(as.numeric(picks))]$cuisine
        results[i] <- sum(found==actual)
}
summary(results)

### Debug case 2
cat(found)
cat(actual)
# Filipino considered to be Southern US
picks[99]
answers[J(25195)]
testers[J(25195)]$ingredients
probs["mango",]
# Actually mango is mexican, so how come it's dropped?
## Hypothesis - it's zeroed by some ingridient that is only used in Southern US
## So zeroing this way isn't probably a good way to go.