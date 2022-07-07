

#####################################
# KEY INSIGHTS
#####################################
# STATISTIC              CRITERION
#-----------------------------------
# R-Squared          Higher the better 
# Adj R-Squared      Higher the better
# F-Statistic        Higher the better
# Std. Error         Closer to zero the better
# t-statistic        Should be greater 1.96 for p-value to be less than 0.05




# Install xlsx package in R
#install.packages("xlsx")        # comment the this line once you have installed it


# Load xlsx package in R
library("xlsx")         

# example of working with directories
getwd()                         # get the working directory

setwd("~/Desktop/")             # set the working directory

getwd()                         # get the working directory checking it



############################################
# Reading data from an Excel xlsx file
############################################
# choose the file and excel to import data from
# the 1 represents the excel sheet to read in. Here we are reading in the first sheet

carData  <- read.xlsx(file.choose(), 1)  # read the the file and store it in a dataframe

names(carData)                  # get the columns names of the dataframe


############################################
# What effect price
############################################

# Models for lm are specified symbolically. A typical model has the form response 
# ~ terms where response is the (numeric) response vector and terms is a series 
# of terms which specifies a linear predictor for response. 

# the model estimated below:
# Price = b_0 + b_1 MPG.city +  b_2 MPG.highway + b_3 Cylinders + b_4EngineSize 
#         + b_5 Horsepower  + b_6 RPM + b_7 Rev.per.mile + b_8 Fuel.tank.capacity 
#         + b_9 Passengers + b_10 Length + b_11 Wheelbase + b_12 Width + b_13 Turn.circle 
#         + b_14 Rear.seat.room + b_15 Luggage.room  + B_16 Weight

model1 <- lm(Price ~ MPG.city +  MPG.highway + Cylinders + EngineSize + Horsepower 
                  + RPM + Rev.per.mile + Fuel.tank.capacity + Passengers + Length
                  + Wheelbase + Width + Turn.circle + Rear.seat.room + Luggage.room 
                  + Weight, data = carData)

summary(model1)                         # output the information of the the estimated model

model1_Stats <- summary(model1)         # store the model information

names(model1_Stats )                    # get an insight of the fields in the store model information


model1_Stats$adj.r.squared              # example how to get a piece of data from the store model information


##############################################
# Model 2 - Refinement
##############################################
# there are many ways to refine the model......

# simple method ....
# remove all non signficate regressors with a P-value > 0.7 from model 1

model2 <- lm(Price ~    Cylinders + EngineSize + Horsepower 
             + RPM + Rev.per.mile + Fuel.tank.capacity + Length
             + Wheelbase + Width + Turn.circle 
             + Weight, data = carData)

summary(model2)

plot(model2$residuals)


# remove all non significant regression with a P-value > 0.7
model3 <- lm(Price ~    Cylinders +  Horsepower 
             + RPM + Rev.per.mile + Fuel.tank.capacity + Length
             + Wheelbase + Width + Turn.circle 
             , data = carData)

summary(model3)


# remove all non significant regression with a P-value > 0.2
model4 <- lm(Price ~    Cylinders +  Horsepower 
             + RPM  + Length
             + Wheelbase + Width + Turn.circle 
             , data = carData)

summary(model4)


# remove all non significant regression with a P-value > 0.2
model5 <- lm(Price ~     Horsepower + RPM  
             + Wheelbase + Width + Turn.circle 
             , data = carData)

summary(model5)

finalData <- data.frame(carData[ , c("Price" ,"Horsepower", "RPM", "Wheelbase", "Width", "Turn.circle")] )
names(finalData)

pairs(finalData)

names(model5)



plot(model5$residuals)

#calculate MSE


installed.packages('e1071')
library(e1071)


par(mfrow=c(2, 2)) 


# Model-1

plot(density(model1$residuals), 
     main="Density Plot: Model 1 Residuals", 
     ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(model1$residuals), 2))) 

polygon(density(model1$residuals), col="red")


plot(density(model2$residuals), 
     main="Density Plot: Model 2 Residuals", 
     ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(model2$residuals), 2))) 

polygon(density(model2$residuals), col="red")

plot(density(model3$residuals), 
     main="Density Plot: Model 3 Residuals", 
     ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(model3$residuals), 2))) 

polygon(density(model3$residuals), col="red")



plot(density(model5$residuals), 
     main="Density Plot: Model 5 Residuals", 
     ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(model5$residuals), 2))) 

polygon(density(model5$residuals), col="red")


# The R function shapiro.test() can be used to perform the Shapiro-Wilk test 
# From the output, the p-value > 0.05 implying that the distribution of the data 
# are not significantly different from normal distribution. 
# If this is the case we can assume  normality.

shapiro.test(model5$residuals)


summary(model5)


###############################################################################
# Manual Sampling
###############################################################################
# Create Training and Test data 
# Example of a 80-20 split

# setting seed to reproduce results of random sampling
set.seed(100)  

# row indices for training data
trainingRowIndex <- sample(1:nrow(finalData), 0.8*nrow(finalData)) 

# model training data
trainingData <- finalData[trainingRowIndex, ] 

# test data
testData  <- finalData[-trainingRowIndex, ]   


# build the model
trainingModel <- lm(Price ~     Horsepower + RPM  
                    + Wheelbase + Width + Turn.circle 
                    , data = trainingData)


summary(trainingModel)

# predict trainingModel on testing data
distPred <- predict(trainingModel, testData)  


#######################################
# Calculate prediction accuracy and error rates
#######################################

# make actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(index = seq(1: nrow(testData)), 
                                  actuals= testData$Price,
                                  predicteds=distPred))  

# A higher correlation accuracy implies that the actuals and predicted 
# values have similar directional movement,
 cor(actuals_preds$actuals,actuals_preds$predicteds)  

library(ggplot2)


gg <- ggplot(data = actuals_preds, aes(index))  + 
        geom_point(aes(y = actuals), color = "red") + 
        geom_point(aes(y = predicteds), color = "blue") +
        labs( title = "Actual vs Predicted Values")
gg



#############################################################################
#  Many Sample Sets
#  k-fold cross validation
#############################################################################
install.packages('caret')
library(caret)

controlled <- trainControl(method = "cv", number = 5)
control <- lm(Price ~     Horsepower + RPM  
             + Wheelbase + Width + Turn.circle 
             , data = carData)

nrow(finalData)



