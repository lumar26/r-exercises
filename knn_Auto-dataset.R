library(ISLR)

?Auto

ds <- Auto

summary(ds)
str(ds)

summary(ds$name)

# ispitujem potrosnju goriva, tj koliko milja predje po galonu
# pretvaram je u faktorsku izlaznu varijablu u 2 nivoa, da li je potrosnja visoka ili ne

third.quartile <- quantile(ds$mpg, 0.75, names = F)
third.quartile

?quantile

first.quartile <- quantile(ds$mpg, 0.25, names = F)
first.quartile


boxplot(ds$cylinders)
summary(ds$cylinders)
summary(Auto$cylinders)

?scale
summary(scale(Auto$cylinders, center = median(Auto$cylinders), scale = IQR(Auto$cylinders)))


?factor
?cut

min(ds$mpg)
c(min(ds$mpg), first.quartile, third.quartile)
# ds$HighConsumption <- cut(include.lowest = T, x = ds$mpg, breaks = c(min(ds$mpg), first.quartile, third.quartile, max(ds$mpg)), labels = c('Low', 'Moderate', 'High'))

ds$HighConsumption <- ifelse(ds$mpg < first.quartile, 'Yes', 'No')

round(prop.table(table(ds$HighConsumption)), 4)


# izbacujemo onu varijablu na osnovu koje smo kreirali izlaznu
ds$mpg <- NULL

apply(ds[,1:7], 2, shapiro.test)
# ni jedna varijabla nema normalnu raspodelu, tako da za sve radimo standardizaciju sa medijanom i iqr

summary(ds)


# bitno: skaliranje se radi po kolonama, a ne po redovima
ds[,1:7] <- apply(ds[,1:7], 2, 
      function(x) {
  scale(x, center =  median(x), scale = IQR(x))
})

# pravljenje train i test dataseta
library(caret)
set.seed(20)

?createDataPartition
indices <- createDataPartition(ds$HighConsumption, p = 0.8, list = F)

train <- ds[indices,]
test <- ds[-indices,]

# kreiranje modela
library(class)

#knn.pred <- knn(train[,1:7], test[,1:7], cl = train$HighConsumption, k = 3)
# no missing values are allowed - error

train$HighConsumption

knn.pred <- knn(train = train[,1:7], test = test[,1:7], cl = train$HighConsumption, k = 3)
knn.pred
summary(train)

cm1 <- table(actual = test$HighConsumption, tested = knn.pred)
cm1

# interesna sfera su nam vozila koja imaju visoku potrosnju

compute.eval.metrics <- function(cm){
  tp <- cm[2,2]
  tn <- cm[1,1]
  fp <- cm[1,2]
  fn <- cm[2,1]
  a <- sum(diag(cm)) /sum(cm)
  p <- tp / (tp + fp)
  r <- tp / (tp + fn)
  f1 <- 2*r*p/(p+r)
  c(accuracy = a, precision = p, recall = r, F1 = f1)
}

compute.eval.metrics(cm1)

# nista ne valjaju rezultati, tako da mora krosvalidacija jedna da se odradi

library(e1071)

folds <- trainControl(method = "cv", number = 10) 
kGrid <- expand.grid(.k =  seq(from = 3, to = 33, by = 2))
train(HighConsumption ~ cylinders + displacement + horsepower + weight + acceleration + year + origin,
      data = train,
      method = 'knn',
      trControl = folds,
      tuneGrid = kGrid)

new.knn.pred <- knn(train = train[,1:7], test = test[,1:7], cl = train$HighConsumption, k = 7)

cm2 <- table(actual = test$HighConsumption, tested = new.knn.pred)
cm2

compute.eval.metrics(cm2)

data.frame(rbind(compute.eval.metrics(cm1), compute.eval.metrics(cm2)), row.names = c('model with k = 3 -->', 'model with k = 7 -->'))
