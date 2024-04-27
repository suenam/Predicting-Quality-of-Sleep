data <- read.csv("Sleep_health_and_lifestyle_dataset.csv")
View(data)

# encoding the categorical variable
data$Occupation <- as.factor(data$Occupation) 
data$Gender <- as.factor(data$Gender)
data$Sleep.Disorder <- as.factor(data$Sleep.Disorder)
data$BMI.Category <- as.factor(data$BMI.Category)

# creating the linear regression model
sleep.model <- lm(Quality.of.Sleep ~.-Blood.Pressure-Person.ID, data = data)

# summary to view the model's statistics
summary(sleep.model)

# diagnostic plots for regression
par(mfrow = c(1, 1))
plot(sleep.model)

# predict and evaluate
predict_quality <- predict(sleep.model, newdata = data) 

# evaluate with R-squared and Mean Squared Error
rsquared <- summary(sleep.model)$r.squared  # R-squared value
print(paste("R-squared:", rsquared))

# mean Squared Error
mse <- mean((predict_quality - data$Quality.of.Sleep)^2)  # MSE
print(paste("Mean Squared Error:", mse))