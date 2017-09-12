my_data <- read.csv("https://ibm.box.com/shared/static/q0gt7rsj6z5p3fld163n70i65id3awz3.csv")
head(my_data)
str(my_data)
plot(my_data$Access_to_Sanitation, my_data$Life_Expectancy, xlab = "Access to Sanitation (% of population)",
ylab = "Life Expectancy (years)", col = "blue", lwd = 2)
my_data <- my_data[order(my_data["Access_to_Sanitation"]),]
barplot(my_data[c(1:20),"Access_to_Sanitation"],
names.arg = as.vector(my_data[c(1:20),"Country"]),
col = "red", las = 2,
ylab = "Access to Sanintation (% of Population)")
# Order rows increasingly by Life Expectancy
my_data <- my_data[order(my_data["Life_Expectancy"]),]
barplot(my_data[c(1:20),"Life_Expectancy"],
names.arg = as.vector(my_data[c(1:20),"Country"]),
col = "blue", las = 2,
ylab = "Life Expectancy (years)")
str(my_data)
sanitation <- as.vector(my_data$Access_to_Sanitation)
model <- lm(Life_Expectancy ~ sanitation, data=my_data)
summary(model)
model
model <- lm(Life_Expectancy ~ sanitation, data=my_data)
model
model <- lm(Life_Expectancy ~ sanitation, data=my_data)
model
summary(model)
plot(my_data$Access_to_Sanitation, my_data$Life_Expectancy, xlab = "Access to Sanitation (% of Population)",
ylab = "Life Expectancy (years)", col = "blue", lwd = 2)
abline(model, col = "red")
summary(my_data$Access_to_Sanitation)
pointsToPredict <- data.frame(sanitation = c(10, 42))
#Use predict() to compute our predictions!
predictionWithInterval <- predict(model, pointsToPredict, interval = 'prediction')
predictionWithInterval
# Plot the previous scatter plot
plot(my_data$Access_to_Sanitation, my_data$Life_Expectancy, xlab = "Access to Sanitation (% of Population)",
ylab = "Life Expectancy (years)", col = "blue", ylim=c(45, 85), lwd = 2)
# Add the new predicted points!
points(pointsToPredict$sanitation, predictionWithInterval[,"fit"], col = "red", lwd = 4)
points(pointsToPredict$sanitation, predictionWithInterval[,"lwr"], col = "firebrick4", lwd = 4)
points(pointsToPredict$sanitation, predictionWithInterval[,"upr"], col = "firebrick4", lwd = 4)
legend("topleft",legend = c("Dataset Points", "Prediction Points"), fill = c("blue","red"), bty = "n")
