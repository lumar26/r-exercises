##########################
# Naive Bayes Classifier
##########################

# load utility functions
source('util.R')

# get adapted Carseats data set
ds <- get_adapted_carseats_dataset()

#################################
# Discretise numerical variables
#################################

# select numerical variables
str(ds)

# apply the Shapiro-Wilk test to each numerical column (variable)
num.cols <- (sapply(ds, is.numeric))
num.cols
ds[,num.cols]

apply(ds[,num.cols], 2, shapiro.test)

# Normal distribution: CompPrice, Price 

#install.packages('bnlearn')
# load bnlearn package
library(bnlearn)

# open the docs for the discretize f.
?discretize

# select variables to be discretized
to.discretize <- c("Education", "Age", "Population", "Advertising", "Income")

# discretize all variables into 5 bins each
discretized.ds <- discretize(ds[,to.discretize],
           method = "quantile",
           breaks = c(5, 5, 5, 5, 5))

# check the summary of the discretized variables
summary(ds[,to.discretize])

# examine the distribution of the Advertising variable
# by plotting its histogram
library(ggplot2)
ggplot(ds, 
       aes(x = Advertising)) + 
  geom_histogram(bins = 30)

# discretize all variables into 5 bins each, but the Advertising variable 
# into 3 bins
to.discretize
discretized.ds <- discretize(ds[,to.discretize],
                             method = "quantile",
                             breaks = c(5, 5, 5, 2, 5))

discretized.ds

# print the summary statistics of the discretized variables
summary(discretized.ds)

# create a vector of variable names to be added to the data frame with the 
# discretised variables
# merge the discretized data frame with other columns from the original data frame
?merge
ready.ds <- cbind(discretized.ds, ds[,c(1,5,6,9,10,11)])



# update the variable order (optional)

#############################################
# Split the data into training and test sets
#############################################

# load the caret package
library(caret)

# set seed and create train and test sets
set.seed(10)

ind <- createDataPartition(ready.ds$HighSales, p = 0.8, list = F)
train <- ready.ds[ind, ]
test <- ready.ds[-ind, ]

##########################
# Model building
##########################

# load the e1071 package
library(e1071)

# open the docs for the naiveBayes f.
?naiveBayes

# build a model with all variables
nb1 <- naiveBayes(HighSales ~ .,
                  data = train)

nb1

# print the model
print(nb1)

# make the predictions with nb1 model over the test dataset
nb1.prediction <- predict(object = nb1, 
                          newdata = test,
                          type = 'class') 
length(nb1.prediction)


# print several predictions
head(nb1.prediction)

# create the confusion matrix
cm1 <- table(actual = test$HighSales, predicted = nb1.prediction)
cm1

# compute the evaluation metrics
compute_eval_metrics(cm1)

# build a model with variables that proved relevant in the decision tree classifier (Lab #4)
# namely ShelveLoc, Price, Advertising, Income, Age, and US
nb2 <- naiveBayes(HighSales ~ ShelveLoc + Price +  Advertising + Income +  Age + US,
                  data = train)

# make the predictions with nb2 model over the test dataset
nb2.prediction <- predict(object = nb2, 
                          newdata = test,
                          type = 'class') 


# create the confusion matrix for nb2 predictions
cm2 <- table(actual = test$HighSales, predicted = nb2.prediction)
cm2
# compute the evaluation metrics for the nb2 model
compute_eval_metrics(cm2)


# compare the evaluation metrics for nb1 and nb2
comparison.fd <- data.frame(rbind(compute_eval_metrics(cm1), compute_eval_metrics(cm2)),
                            row.names = c("model_1 -> ", "model_2 -> "))

comparison.fd

##############
# ROC curves
##############

# compute probabilities for each class value for the observations in the test set
nb2.prediction <- predict(object = nb2, 
                          newdata = test,
                          type = 'raw')


#install.packages('pROC')
# load pROC package
library(pROC)

# create a ROC curve
roc.curve <- roc(response = test$HighSales,
                 predictor = nb2.prediction[,1])

# print the Area Under the Curve (AUC) value
roc.curve$auc


# plot the ROC curve, using the "youden" method
plot.roc(roc.curve,
         print.thres = T,
         print.thres.best.method = "youden")

# get the coordinates for all local maximas
roc.curve


# choose a threshold that assures a high value for sensitivity 
nb2.coords <- coords(roc.curve,
                     ret = c("accuracy", "spec", "sens", "thr"),
                     x = 'best')
# zbog ovakvog zahteva se koristi 'local maximas' jer ce za svaku metriku da nam izdvoji koliki je 
# treshold pri njenoj najvecoj vrednosti 

nb2.coords$threshold



# create predictions based on the new threshold
nb2.prediction[1,]

nb2.prediction.new <- ifelse(test = nb2.prediction[,1] > nb2.coords$threshold,
                         yes = "No", 
                         no = "Yes")

head(nb2.prediction.new)
nb2.prediction.new <- as.factor(nb2.prediction.new)
# create the confusion matrix for the new predictions

# compute the evaluation metrics
cm3 <- table(actual = test$HighSales, predicted = nb2.prediction.new)
compute_eval_metrics(cm3)



# compare the evaluation metrics for all three models
data.frame(rbind(
  compute_eval_metrics(cm1),
  compute_eval_metrics(cm2),
  compute_eval_metrics(cm3)
), 
row.names = c("model 1 -> ","model 2 -> ", "model 3 -> "))
