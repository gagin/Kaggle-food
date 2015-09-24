answers <- data.table(id=sapply(f, "[[", 1), cuisine=sapply(f, "[[", 2))
testers <- data.table(id=sapply(f, "[[", 1), ingredients=sapply(f, "[[", 3))
testers[, ingredients := sapply(ingredients, sapply, cleanup)]
setkey(testers,id)
        
sample.size <- 20
picks <- sample(unique(data[, 1]), sample.size)
found <- sapply(picks,
       function(x)
               CuisineByIngredients(testers[id==x, 2, with=FALSE][[1]][[1]]))
cat(found)
actual<-answers[id %in% picks]$cuisine
cat(actual)
sum(found==actual)
