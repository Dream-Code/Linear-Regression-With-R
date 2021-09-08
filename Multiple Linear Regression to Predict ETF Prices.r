# Group Project - Self-work

options(warn = -1)

# Getting rid of the char values that mean nothing and removing NAs
etfRaw <- ETFs_Final[, -c(1,2)]
etfRaw <- etfRaw[complete.cases(etfRaw), ]

# Viewing initial plots for linearity
plot(etfRaw)

# Searching for negative mean returns
summary(etfRaw)

# Seeing the correlation of all variables
cor(etfRaw)

# Building the first model with all variables
etfRawModel <- lm(fund_return_2018 ~ fund_return_2017 + fund_return_2016 + fund_return_2015 + 
                    fund_return_2014 + fund_return_2013, data = etfRaw)
summary(etfRawModel)

# Building model with just 2015 for Week 7 - Milestone 1
etfMilestoneOne <- lm(fund_return_2018 ~ fund_return_2015, data = etfRaw)
summary(etfMilestoneOne)
plot(etfRaw$fund_return_2015, etfRaw$fund_return_2018)
cor(etfRaw$fund_return_2015, etfRaw$fund_return_2018)
####################### Initial Model #############################
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      -4.92026    0.68540  -7.179 1.82e-12 ***
# fund_return_2017 -0.15581    0.02012  -7.744 3.43e-14 ***
# fund_return_2016 -0.01236    0.01925  -0.642    0.521    
# fund_return_2015  0.25449    0.03058   8.321 4.66e-16 ***
# fund_return_2014  0.22896    0.02361   9.698  < 2e-16 ***
# fund_return_2013 -0.07816    0.01352  -5.782 1.12e-08 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 7.03 on 691 degrees of freedom
# Multiple R-squared:  0.3626,	Adjusted R-squared:  0.358 
# F-statistic: 78.61 on 5 and 691 DF,  p-value: < 2.2e-16
###################################################################


###################################################################
#################### N-Fold Cross validation ######################
###################################################################
######### Using DAAG to cross validate with K-Fold method #########
###################################################################
install.packages("DAAG")
library(DAAG)

# Pass dataframe, pass form(ula) of linear model for price vs dataset, m = folds
outETF <- cv.lm(data = etfRaw, form.lm = formula(fund_return_2018 ~ .), 
             plotit = "Observed", m = 10)
summary(outETF)

###################################################################
###################### Stepwise Regression ########################
###################################################################
install.packages("MASS")
library(MASS)


####   FORWARD ELIMINATION  ####
etfSTEP <- etfRaw[, -c(1,2)]
model_full <- lm(fund_return_2018 ~ ., data = etfSTEP)
model_empty <- lm(fund_return_2018 ~ 1, data = etfSTEP)
summary(model_empty)

step <- stepAIC(model_empty, direction = "forward", 
                scope = list(upper = model_full, lower = model_empty))
summary(step)
# Best Model by AIC that includes 2015:
# fund_return_2018 ~ fund_return_2014 + fund_return_2017 + fund_return_2015


###################################################################
###################### RESIDUAL ANALYSIS ##########################
###################################################################
model <- lm(fund_return_2018 ~ fund_return_2015, data = etfRaw)
summary(model)

model$residuals

# We want this to be 0 or close to 0
sum(model$residuals)

# We want this to be normal distributed - There are some outliers
hist(model$residuals, breaks = 100)

# Normalizing the residuals with the z-score
mean = mean(model$residuals)
sd = sd(model$residuals)
resid_zscore = (model$residuals - mean) / sd

# Now showing a normally distributed curve for residuals
hist(resid_zscore, breaks = 100)

library(car)

# Testing for autocorrelation with independent variables
help("durbinWatsonTest")
durbinWatsonTest(model)

plot(model)

fundSQ = etfRaw$fund_return_2015 * etfRaw$fund_return_2015
model <- lm(fund_return_2018 ~ fund_return_2015 + log(fundSQ), data = etfRaw)
summary(model)

plot(model)
































