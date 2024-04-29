library(tree)
library(randomForest)

# import data
data <- read.csv("Sleep_health_and_lifestyle_dataset.csv")

# clean data
data$Occupation <- as.factor(data$Occupation)
data$Gender <- as.factor(data$Gender)
data$Sleep.Disorder <- as.factor(data$Sleep.Disorder)
data$BMI.Category <- as.factor(data$BMI.Category)

# DECISION TREE -----------------
originalTreeMSE <- rep(0, 10)
for (i in 1:10) {
  set.seed(i)
  
  # split into train and test sets
  sample <- sample(nrow(data), nrow(data)*0.80)
  train <- data[sample,]
  test <- data[-sample,]
  
  # build tree
  tree <- tree(Quality.of.Sleep ~ .-Blood.Pressure-Person.ID, data=train)
  
  # mse
  prediction.original <- predict(tree, test)
  mse.original <- mean((prediction.original - test$Quality.of.Sleep)^2)
  originalTreeMSE[i] <- mse.original
}
originalTreeMSE
mean(originalTreeMSE)

# display tree
summary(tree)
plot(tree)
text(tree, pretty = 0)

# find r^2 of decision tree
dataMean <- mean(test$Quality.of.Sleep)
SSTotal <- sum((test$Quality.of.Sleep - dataMean)^2)
originalSSResidual <- sum((test$Quality.of.Sleep - prediction.original)^2)
originalR2 <- 1 - (originalSSResidual / SSTotal)
originalR2


# PRUNED TREE -----------------
prunedTreeMSE <- rep(0, 10)
for (i in 1:10) {
  set.seed(i)
  
  # split into train and test sets
  sample <- sample(nrow(data), nrow(data)*0.80)
  train <- data[sample,]
  test <- data[-sample,]
  
  # tree pruning cross validation
  cv <- cv.tree(tree)
  
  # build pruned
  tree.pruned <- prune.tree(tree, best=3)
  
  # mse
  prediction.pruned <- predict(tree.pruned, test)
  mse.pruned <- mean((prediction.pruned - test$Quality.of.Sleep)^2)
  prunedTreeMSE[i] <- mse.pruned
}
prunedTreeMSE
mean(prunedTreeMSE)

# plot cv deviance
plot(cv$size, cv$dev, type = "b")

# display pruned tree
summary(tree.pruned)
plot(tree.pruned)
text(tree.pruned, pretty = 0)

# find r^2 of pruned tree
dataMean <- mean(test$Quality.of.Sleep)
SSTotal <- sum((test$Quality.of.Sleep - dataMean)^2)
prunedSSResidual <- sum((test$Quality.of.Sleep - prediction.pruned)^2)
prunedR2 <- 1 - (prunedSSResidual / SSTotal)
prunedR2


# RANDOM FOREST -----------------
randomForestMSE <- rep(0, 10)
for (i in 1:10) {
  set.seed(i)
  
  # split into train and test sets
  sample <- sample(nrow(data), nrow(data)*0.80)
  train <- data[sample,]
  test <- data[-sample,]
  
  # build random forest of 500 trees
  random.forest <- randomForest(
    Quality.of.Sleep ~ .-Blood.Pressure-Person.ID,
    data = train,
    ntree = 500,
    mtry = 3,
    importance = TRUE,
  )
  
  # mse
  prediction.random.forest <- predict(random.forest, test)
  mse.random.forest <- mean((prediction.random.forest - test$Quality.of.Sleep)^2)
  randomForestMSE[i] <- mse.random.forest
}
randomForestMSE
mean(randomForestMSE)

# importance plot
varImpPlot(random.forest)

# plot tree
random.forest.tree <- tree(Quality.of.Sleep ~ .-Blood.Pressure-Person.ID, data = train, subset = random.forest$inbag)
plot(random.forest.tree)
text(random.forest.tree, pretty = 0)

# find r^2 of random forest
dataMean <- mean(test$Quality.of.Sleep)
SSTotal <- sum((test$Quality.of.Sleep - dataMean)^2)
randomForestSSResidual <- sum((test$Quality.of.Sleep - prediction.random.forest)^2)
randomForestR2 <- 1 - (randomForestSSResidual / SSTotal)
randomForestR2
