#=============================================================================
# PROGRAMMER: Alwahab Mohammad
# PANTHER ID: 6130253
#
# CLASS: CAP 4830
# SECTION: U01
# SEMESTER: FALL 2021
# CLASSTIME: T/TH 12:30-1:45 pm

# CERTIFICATION: I understand FIU's academic policies, and I certify that this 
#                work is my own and that none of it is the work of any other person.

#=============================================================================

modelData  <- read.xlsx(file.choose(), 1)  # read the the file and store it in a dataframe

names(modelData)

model1  <- lm(UNRATE_PCH ~ DFII10_PCH + CPILFESL_PCH + XTEITT01CNM156S_PCH 
              +  DCOILWTICO_PCH + PCOPPUSDM_PCH + PCE_PCH 
              + WPU101_PCH + GPDIC1_PCH + RRVRUSQ156N_PCH, data = modelData)

summary(model1)

plot(density(model1$residuals), 
     main="Density Plot: Model 1 Residuals", 
     ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(model1$residuals), 2))) 

polygon(density(model1$residuals), col="red")

shapiro.test(model1$residuals)

model2  <- lm(UNRATE_PCH ~ DFII10_PCH + XTEITT01CNM156S_PCH 
              +  DCOILWTICO_PCH + PCOPPUSDM_PCH + PCE_PCH 
              + WPU101_PCH + GPDIC1_PCH, data = modelData)

summary(model2)


#######################################
# Calculate prediction accuracy and error rates
#######################################


model2Data <-
  data.frame(modelData[, c(
    "UNRATE_PCH" ,
    "DFII10_PCH" ,
    "XTEITT01CNM156S_PCH" ,
    "DCOILWTICO_PCH" ,
    "PCOPPUSDM_PCH" ,
    "PCE_PCH" ,
    "WPU101_PCH" ,
    "GPDIC1_PCH"
  )])

distPred <- predict(model2, model2Data)

# make actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(
    index = seq(1:nrow(model2Data)),
    actuals = model2Data$UNRATE_PCH,
    predicteds = distPred))
  
  cor(actuals_preds$actuals , actuals_preds$predicteds)
  
  library(ggplot2)
  
  
  gg <- ggplot(data = actuals_preds, aes(index))  +
    geom_point(aes(y = actuals), color = "red") +
    geom_point(aes(y = predicteds), color = "blue") +
    labs(title = "Actual vs Predicted Values")
  gg
  
#####
model3  <- lm(UNRATE_PCH ~ DFII10_PCH + XTEITT01CNM156S_PCH 
              +  DCOILWTICO_PCH + PCE_PCH 
              , data = modelData)

summary(model3)


model3Data <-
  data.frame(modelData[, c(
    "UNRATE_PCH" ,
    "DFII10_PCH" ,
    "XTEITT01CNM156S_PCH" ,
    "DCOILWTICO_PCH" ,
    "PCE_PCH"
  )])

set.seed(777)

trainingRowIndex <- sample(1:nrow(model3Data), 0.6*nrow(model3Data)) 

# model training data
trainingData <- model3Data[trainingRowIndex, ] 

# test data
testData  <- model3Data[-trainingRowIndex, ]   


# build the model
model4 <- lm(UNRATE_PCH ~ DFII10_PCH + XTEITT01CNM156S_PCH 
                       +  DCOILWTICO_PCH + PCE_PCH 
                       , data = trainingData)


summary(model4)

# predict model4 on testing data
distPred <- predict(model4, testData)  
head(distPred)

actuals_preds <- data.frame(cbind(index = seq(1: nrow(testData)), 
                                  actuals= testData$UNRATE_PCH,
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


install.packages('caret')
library(caret)

controlled <- trainControl(method = "cv", number = 10)
control <- lm(UNRATE_PCH ~ DFII10_PCH + XTEITT01CNM156S_PCH 
              +  DCOILWTICO_PCH + PCE_PCH 
              , data = trainingData)

nrow(model3Data)

