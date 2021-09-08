# Name: Steven Rivera
# Date: 08/04/2021
# Project Name: College Admission Prediction

# This R language project uses the admit data provided in DSC 423 Regression 
# Analysis taught by Dr. Jonathan Gemmell a professor of computer science and 
# data science at DePaul University

## Initial summary of the dataset ##
summary(admit)

hist(admit$gre)
hist(admit$gpa)
plot(admit)

# The rank and admit variables are INTs initially and need to be changed
# to factors so we have levels/dummy variables to work with
admit$rank <- factor(admit$rank)
admit$admit <- factor(admit$admit)

# Building the general linear model (GLM) manually since there are only 
# three(3) independent variables. Also, classifying the family as binomial
# so the glm function knows we are working with logistic regression on a binary 
# dependent variable
model <- glm(admit ~ gre + gpa + rank, data = admit, family = "binomial")

# Since GRE in the first model had a P-Value of 0.038 it can be accepted
# in a standard 5% alpha, however I created a second model without this value
# just to determine the effects on removing. While there is a positive shift
# in the P-Values, and a change from 470 to 472.88 AIC value I determine here 
# that it is not a significant enough change in output  to keep GRE out of the 
# model.
model2 <- glm(admit ~ gpa + rank, data = admit, family = "binomial")

# Summary of the model(s)
summary(model)
summary(model2)

# Confidence intervals of the model
confint(model)

# Coefficients of the model raises to the e power and then subtracts 1. 
# This effectively 'de-logs' the output
# No longer log odds, now just the odds
exp(coef(model))-1

# Loading packages for Confusion Matrix to test model robustness and validity
install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 
                   'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))
install.packages("InformationValue")
install.packages("ISLR")
library(caret)
library(InformationValue)
library(ISLR)


# Initializing a training and testing set with a 50/50 probability test
set.seed(1)
sample <-sample(c(TRUE, FALSE), nrow(admit), replace=TRUE, prob=c(0.5,0.5))
train <- admit[ sample, ]
test <-  admit[!sample, ]

testModel <- glm(admit ~ gre + gpa + rank, data = admit, family = "binomial")
summary(testModel)

# Predict probability of admittance using testModel
admitPredict <- predict(testModel, test, type = "response")

# Converts the default responses from Yes/No to 1 and 0 --> Not needed?
# test$admit <- ifelse(test$admit == "Yes", "No" , 1, 0)

# Finding the optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(test$admit, admitPredict)[1]

# Build the confusion matrix
confusionMatrix(test$admit, admitPredict)

# True positive rate - % the model predicted wouldn't get accepted
sensitivity(test$admit, admitPredict)

# True negative rate - % model predicted would get accepted
specificity(test$admit, admitPredict)

# Total rate - % of total incorrect classifications of the model
misClassError(test$admit, admitPredict, threshold = optimal)


#### Analysis of the above output ####
  ## Direct analysis of summary on the model ##

   # All of our P-Values look good and pass the test using a 5% alpha
   # AIC can be ignored here because it is relative to model2
   # GRE == 0.002264: for every unit change in GRE, the log odds of admittance 
   # changes by 0.002264 OR .2%

   # GPA ==  0.804038: for every unit change in GPA, the log odds of admittance 
   # changes by 80.4%

 ## Analysis after delogging ##
  # GRE: No change from above, the probability of acceptance is still .2% for
  # every unit change in GRE scores - Unable to know what a level is in the GRE
  # scoring table, yet it is a minuscule admittance advantage per our model

  # GPA: For every unit change in GPA, the probability of acceptance increases 
  # by 124%, which should be considered valid. There is a highly measurable 
  # difference changing from 2.0 to 3.0 in terms of admission criteria and we
  # can expect students with higher GPAs to have a higher chance of admittance

  # Rank2-Rank4: the output here shows that a student at a rank 1 school has a
  # much higher chance of admittance than a rank 2-4 school and the odds of a
  # rank 4 school decrease by 78.8% all other factors being equal

