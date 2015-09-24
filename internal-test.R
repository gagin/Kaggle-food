answers <- data.table(id=sapply(f, "[[", 1), cuisine=sapply(f, "[[", 2))
setkey(answers,id) # It messes up the order
testers <- data.table(id=sapply(f, "[[", 1), ingredients=sapply(f, "[[", 3))
testers[, ingredients := sapply(ingredients, sapply, cleanup)]
setkey(testers,id)
        
# 1000 to make it definitive marker when it only mentioned for a single cuisine
probs.cut <- ifelse(probs==1,1000,(probs-0.1)^3)#ifelse(probs<0.2, 0, probs)
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
#cat(found)
actual<-answers[J(as.numeric(picks))]$cuisine
#cat(actual)
results[i] <- sum(found==actual)
}
summary(results)