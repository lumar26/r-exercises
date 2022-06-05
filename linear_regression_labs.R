##########################
# Linear Regression
##########################

# load MASS, corrplot and ggplot2
library(MASS)
#install.packages('corrplot')
library(corrplot)
library(ggplot2)

# examine the structure of the Boston dataset
?Boston
ds <- Boston

str(ds)

# bring out the docs for the dataset 

# compute the correlation matrix
corr.m <- cor(ds)
corr.m[14, ]

?which

which(corr.m[14,] > 0.5 | corr.m[14,] < -0.5)
which(corr.m[14,] < -0.5)



# one option for plotting correlations: using colors to represent the extent of correlation
# another option, with both colors and exact correlation scores
corrplot.mixed(corr.m, tl.cex = 0.75, number.cex = 0.75)
corrplot.mixed(corr.m)

#  plot *lstat* against the response variable]
ds$lstat
ggplot(data = ds,
       mapping = aes(x = lstat, y = medv)) +
  geom_point() +
  geom_smooth(method = 'lm')

#  plot *rm* against the response variable
ds$lstat
ggplot(data = ds,
       mapping = aes(x = rm, y = medv)) +
  geom_point() +
  geom_smooth(method = 'lm')


#############################################
# Split the data into training and test sets
#############################################

# install.packages('caret')
library(caret)

# assure the replicability of the results by setting the seed 
set.seed(10)

# generate indices of the observations to be selected for the training set
i <- createDataPartition(ds$medv, p = 0.8, list = F)
# select observations at the positions defined by the train.indices vector
train.data <- ds[i,]
train.data
# select observations at the positions that are NOT in the train.indices vector
test.data <- ds[-i,]

##########################
# Simple Linear Regression
##########################

# build an lm model with a formula: medv ~ lstat 
simple.model <- lm(medv ~ lstat, data = train.data)


simple.predict <- predict(simple.model, test.data, interval = 'predict')
simple.predict


# print the model summary
summary(simple.model)

# print all attributes stored in the fitted model 
print(simple.model)

# print the coefficients
simple.model$coefficients

# print the coefficients with the coef() f.
coef(simple.model)



# compute the RSS
RSS <- sum(simple.model$residuals^2)
RSS

# compute 95% confidence interval
confint(simple.model, level = 0.95)
simple.model$fitted.values
names(simple.model)



# plot the data points and the regression line
ggplot(data = train.data, 
       mapping = aes(y = medv, x = lstat)) +
  geom_point() +
  geom_smooth(method = 'lm')


##########################
## Making predictions
##########################

# calculate the predictions with the fitted model over the test data
# calculate the predictions with the fitted model over the test data, including the confidence interval
pred1 <- predict(object = simple.model, 
                 newdata = test.data, 
                 interval = 'confidence')

pred1


# calculate the predictions with the fitted model over the test data, including the prediction interval

pred2 <- predict(object = simple.model, 
                 newdata = test.data, 
                 interval = 'predict')

pred2

##########################
## Diagnostic Plots
##########################

# split the plotting area into 4 cells
?par
par(mfrow = c(2, 2))

# print the diagnostic plots
plot(simple.model)

# reset the plotting area
par(mfrow = c(1,1))

# compute the leverage statistic
leverages <- hatvalues(simple.model)

# calculate the number of high leverage points 
leverages
plot(leverages)
cutoff <- 2*(1 + 1)/407
cutoff
length(which(leverages > cutoff))


###############################
## Multiple Linear Regression
###############################

# generate the scatterplots for variables medv, lstat, rm, ptratio
which(corr.m[14,] > 0.5 | corr.m[14,] < -0.5)
names(train.data)
pairs(medv ~ lstat + rm +  ptratio,  data = train.data)

# build an lm model with a train dataset using the formula: medv ~ lstat + rm + ptratio
lm1 <- lm(medv ~ lstat + rm +  ptratio, 
          data = train.data)

# print the model summary
summary(lm1)
# calculate the predictions with the lm2 model over the test data
lm1.pred <- predict(lm1, test.data)
plot(lm1.pred)

# print out a few predictions
head(lm1.pred)


# combine the test set with the predictions


# plot actual (medv) vs. predicted values
ggplot(data = cbind(test.data, pred = lm1.pred)) +
  geom_density(mapping = aes(x = medv, color = 'real')) +
  geom_density(mapping = aes(x = pred , color = 'predicted')) +
  theme_classic()
  


# calculate RSS
RSS <- sum((lm1.pred - test.data$medv)^2)

# calculate TSS
TSS <- sum((mean(train.data$medv) - test.data$medv)^2)

# calculate R-squared on the test data
R.sq <- 1 - RSS/TSS
R.sq

# calculate RMSE
RMSE <- sqrt(RSS/nrow(test.data))
RMSE

# compare medv mean to the RMSE
RMSE/mean(test.data$medv)


# build an lm model with the training set using all of the variables except chas
# note the use of '.' to mean all variables and the use of '-' to exclude the chas variable
lm3 <- lm(medv ~ . - chas, data = train.data)
summary(lm3)

# print the model summary

# check for multicolinearity using the vif function (from the 'car' package)
library(car)


# calculate vif

# calculate square root of the VIF
sort(sqrt(vif(lm3)))

# build an lm model with the training set using all of the variables except chas and tax
# (multicolinearity was detected for 'tax') 
lm4 <- lm(medv ~ . - (chas + tax), train.data)
summary(lm4)

# check the VIF scores again
sort(sqrt(vif(lm4)))

# next, we will exclude *nox* and build a new model (lm5):
lm4 <- lm(medv ~ . - (nox + chas + tax), train.data)
summary(lm4)
sort(sqrt(vif(lm4)))
# The *dis* variable is the edge case
summary(lm4)

# The summary of lm5 indicated that *dis* should be excluded
lm6 <- lm(medv ~ . - (nox + chas + tax + dis), train.data)
summary(lm6)

# calculate the predictions with the new model over the test data
lm6.pred <- predict(lm6, test.data)
sort(sqrt(vif(lm6)))
# print out a few predictions


# combine the test set with the predictions



# plot actual (medv) vs. predicted values
ggplot(cbind(test.data, pred = lm6.pred)) +
  geom_density(mapping = aes(x = medv, color = 'real')) + 
  geom_density(mapping = aes(x = pred, color = 'predicted')) + 
  theme_classic()


# calculate RSS
RSS6 <- sum(lm6$residuals^2)
TSS6 <- sum((mean(train.data$medv) - test.data$medv)^2)

# calculate R-squared on the test data
R.sq.6 <- 1 - RSS6/TSS6
R.sq.6

# calculate RMSE
RMSE6 <- sqrt(RSS6/nrow(test.data))
RMSE6



