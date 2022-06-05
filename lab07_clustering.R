###########################
# K-means Clustering
###########################

###########################
# Load and prepare data set
###########################

# load the data from "data/wholesale_customers.csv"
ds <- read.csv("data/customers.csv")


# examine the data structure
str(ds)

# check for missing values
summary(ds)

# laod ggplot2 library
library(ggplot2)

# plot boxplots for all numeric variables
ggplot(ds, aes(x = ds$Channel, y = ds$Fresh, fill = ds$Channel)) + geom_boxplot()
ggplot(ds, aes(x = ds$Channel, y = ds$Milk, fill = ds$Channel)) + geom_boxplot()
ggplot(ds, aes(x = ds$Channel, y = ds$Grocery, fill = ds$Channel)) + geom_boxplot()
ggplot(ds, aes(x = ds$Channel, y = ds$Frozen, fill = ds$Channel)) + geom_boxplot()
ggplot(ds, aes(x = ds$Channel, y = ds$Detergents_Paper, fill = ds$Channel)) + geom_boxplot()
ggplot(ds, aes(x = ds$Channel, y = ds$Delicatessen, fill = ds$Channel)) + geom_boxplot()



# split the dataset into two subsets based on the value of the Channel variable
ds$Channel <- as.character(ds$Channel)
retail.ds <- subset(ds, Channel == 'Retail')
str(retail.ds)
summary(retail.ds)

# print the summary of the retail.data

# remove the Channel variable
retail.ds$Channel <- NULL

# compute the number of outliers for all numeric variables
stats <- boxplot.stats(ds$Fresh)
stats

apply(retail.ds[,-1], 2, function(var) {
  length(boxplot.stats(var)$out)  
})

# install.packages('DescTools')
library(DescTools)

# Start with the *Grocery* variable. 


# Winsorize the Grocary variable using Winsorize f. from DescTools

# Plot the Grocery values after Winsorizing
grocery.w <- Winsorize(retail.ds$Grocery, probs = c(0, 0.95))
boxplot(grocery.w)

# Now, do the same for the *Frozen* variable
frozen.w <- Winsorize(retail.ds$Frozen, probs = c(0, 0.94))
boxplot(frozen.w)



# update *retail.data* with the winsorized values
retail.ds$Frozen <- frozen.w
retail.ds$Grocery <- grocery.w

# print the summary of the retail.data dataset
summary(retail.ds)

##############################
# Clustering with 2 Features
##############################

# print the matrix of scatterplots for all numeric variables

# plot the scatterplot for the variables Frozen and Milk
ggplot(retail.ds, aes(x = Frozen, y = Milk)) + geom_point()


# create a subset of the data with variables Frozen and Milk
simple.ds <- retail.ds[, c('Frozen', 'Milk')]

# print the summary of the new dataset
summary(simple.ds)


# function for performing the normalization
normailze <- function(var) {
  if(sum(var, na.rm = T) == 0) var
  else (var - min(var, na.rm = T)) / (max(var, na.rm = T) - min(var, na.rm = T))
}

# normalize both variables
simple.ds$Frozen <- normailze(simple.ds$Frozen)
simple.ds$Milk <- normailze(simple.ds$Milk)


# print the summary
summary(simple.ds)
ggplot(simple.ds, aes(x = Frozen, y = Milk)) + geom_point()

# open the documentation for the kmeans function
?kmeans

# set the seed to assure replicability of the results
set.seed(123)

?kmeans

# run the clustering with 4 clusters, iter.max=20, nstart=1000
model1 <- kmeans(simple.ds, 4, iter.max = 20, nstart = 1000)

# print the model
model1$totss
model1$centers
model1$size
model1$cluster
model1$betweenss
model1

# add the cluster as a new variable to the dataset
simple.ds$Cluster <- as.factor(model1$cluster)
summary(simple.ds$Cluster)

# print several instances of the dataset


# plot the clusters along with their centroids

ggplot(data= simple.ds, aes(x=Frozen, y=Milk, colour=Cluster)) +
  geom_point() +
  labs(x = "Annual spending on frozen products",
       y = "Annual spending on dairy products",
       title = "Retail customers annual spending") +
  theme_bw() +
  # add cluster centers
  geom_point(data=as.data.frame(model1$centers), colour="black", size=4, shape=17)

#################################
# Selecting the Best Value for K
#################################

# create an empty data frame for storing evaluation measures for different k values
metrics.model1 <- data.frame()


# remove the column with cluster assignments
simple.ds$Cluster <- NULL


# run kmeans for all K values in the range 2:8
for (k in 2:10) {
  kmeans.model <- kmeans(simple.ds, centers = k, iter.max = 20, nstart = 1000)
  metrics.model1 <- rbind(metrics.model1, 
                          c(k, kmeans.model$tot.withinss, kmeans.model$betweenss / kmeans.model$totss)
                          )
}

# assign more meaningful column names
names(metrics.model1) <- c('k', 'within_SS', 'ratio')


# print the evaluation metrics
metrics.model1

# plot the line chart for K values vs. tot.within.ss 
ggplot(data= metrics.model1, aes(x=k, y=within_SS)) +
  geom_line() +
  scale_x_continuous(breaks=seq(from=0, to=10, by=1))

# load the source code from the Utility.R file
source("util.R")

# calculate the difference in tot.within.ss and in ratio for each two consecutive K values
data.frame(K=2:10,
           tot.within.ss.delta=compute.difference(metrics.model1$within_SS),
           ratio.delta=compute.difference(metrics.model1$ratio))

# We'll examine the solution with k=3, as that seems to be the best K value
set.seed(123)
model3 <- kmeans(simple.ds, centers = 3, iter.max = 20, nstart = 1000)
model3$cluster

# store the (factorized) cluster value in a new variable 
simple.ds$Cluster <- model3$cluster

# plot the clusters along with their centroids
ggplot(data= simple.ds, aes(x=Frozen, y=Milk, colour=Cluster)) +
  geom_point() +
  labs(x = "Annual spending on frozen products",
       y = "Annual spending on dairy products",
       title = "Retail customers annual spending") +
  theme_bw() +
  # add cluster centers
  geom_point(data=as.data.frame(model3$centers), colour="black", size=3, shape=17)

# calculate the mean and sd values for all three clusters 


###########################################
# Clustering with several numeric features
###########################################

library(corrplot)
# compute correlations (avoid the Region variable)
cor.m <- cor(retail.ds[,-1])
cor.m

# create correlations plot (as was done in the linear regression lab)
corrplot.mixed(cor.m, tl.cex = .75, number.cex = .75)

# Remove Grocery due to its high correlation with both Detergents_Paper and Milk
retail.ds$Grocery <- NULL

# normalize all numeric variables from the retail.data dataset
summary(retail.ds)
retail.ds.norm <- apply(retail.ds[,2:6], 2, normailze)


# print the summary
summary(retail.ds.norm)


# create an empty data frame to store evaluation measures
evals <- data.frame()


# run kmeans for K values in the range 2:8
for (k in 2:7) {
  model <- kmeans(retail.ds.norm, k, iter.max = 20, nstart = 1000)
  evals <- rbind(evals, c(k, model$withinss, model$betwenss / model$totss))
}

names(evals) <- c('k', 'within_SS', 'ratio')
# assign meaningful column names
evals
# plot the line chart for K values vs. tot.within.ss 
ggplot(data= evals, aes(x=k, y=within_SS)) +
  geom_line() +
  scale_x_continuous(breaks=seq(from=0, to=10, by=1))

# the plot suggests 3 clusters, but we'll also calculate
# the difference in tot.within.ss and in ratio for each two consecutive K values

# set the seed
set.seed(123)

# run the clustering for 3 clusters, iter.max=20, nstart=1000
model10 <- kmeans(retail.ds.norm, 3, iter.max = 20, nstart = 1000)
model10

# calculate and compare summary statistics (mean and st. deviation) for the three clusters 

# plot the distribution of the attributes across the 3 clusters
