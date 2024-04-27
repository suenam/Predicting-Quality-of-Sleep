library(tree)

# import data
data <- read.csv("Sleep_health_and_lifestyle_dataset.csv")
set.seed(1)

# clean data
data$Occupation <- as.factor(data$Occupation)
data$Gender <- as.factor(data$Gender)
data$Sleep.Disorder <- as.factor(data$Sleep.Disorder)
data$BMI.Category <- as.factor(data$BMI.Category)

# split into train and test sets
sample <- sample(nrow(data), nrow(data)*0.80)
train <- data[sample,]
test <- data[-sample,]

# create tree model
tree <- tree(Quality.of.Sleep ~ .-Blood.Pressure-Person.ID, data=train)
summary(tree)
plot(tree)
text(tree)

# helper to understand Occupations in tree model
levels(data$Occupation)

# perform cross validation on tree
cv <- cv.tree(tree)
cv
# plot cv to find the 'elbow' for best model size
plot(cv$size, cv$dev, type = "b")

# found elbow of 3 to prune tree with
tree.pruned <- prune.tree(tree, best=3)
summary(tree.pruned)
plot(tree.pruned)
text(tree.pruned)

# compare original vs pruned tree
prediction.original <- predict(tree, test)
prediction.pruned <- predict(tree.pruned, test)

mse.original <- mean((prediction.original - test$Quality.of.Sleep)^2)
mse.pruned <- mean((prediction.pruned - test$Quality.of.Sleep)^2)

mse.original
mse.pruned
# pruned tree results in a worse mse, suggesting decision tree isn't a good enough model.

# lets compare it with a random forest model...
library(randomForest)

# create random forest model with 500 trees
random.forest <- randomForest(
  Quality.of.Sleep ~ .-Blood.Pressure-Person.ID,
  data = train,
  ntree = 500,
  mtry = 3,
  importance = TRUE 
)
summary(random.forest)
random.forest$importance

# analyze random forest
prediction.random.forest <- predict(random.forest, test)
mse.random.forest <- mean((prediction.random.forest - test$Quality.of.Sleep) ^ 2)

cat("Original Decision Tree MSE:", mse.original, "\n")
cat("Pruned Decision Tree MSE:", mse.pruned, "\n")
cat("Random Forest MSE:", mse.random.forest, "\n")
