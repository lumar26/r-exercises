library(ISLR)
ds <- read.csv('energy_efficiency_data.csv')

str(ds)
length(ds)
ds[,10]
ds[,-10]
summary(ds[,-10])

## cilj je uz pomoc knn algoritma proceniti grejno opterecenje u odnosu na specifikacije zgrade tj. objekta

summary(ds)

# definisanje izlazne faktorske varijable
ds$High.Heating.Load <- ifelse(ds$Heating_Load > quantile(ds$Heating_Load, 0.75),
                               'Yes',
                               'No')
ds$High.Heating.Load <- as.factor(ds$High.Heating.Load)
ds$Heating_Load <- NULL

# varijabla Cooling_load nema uticaja na anse predvidjanje
ds$Cooling_Load <- NULL


# s obzirom na to da opsezi numerickih varijavli nisu uniformi i dosta se razlikuju od varijable do varijable
# potrebno je odraditi standardizaciju jer knn algoritam racuna euklidska rastojanja medju opservacijama, tako da
# varijable koje imaju veci opseg vrednosti mogu nesraymerno vise da uticu na model od ostalih varijabli sa manjim 
# numerickim opsegom

# provera da li neka varijala ima normalnu raspodelu
apply(ds[,-9], 2, shapiro.test)

# ni jedna varijabla nema normalnu raspodelu tako da primenjujemo skaliranje sa medijanom i IQR

ds[,-9] <- apply(ds[,-9], 2, function(x){
  scale(x, center = median(x), scale = IQR(x))
})

summary(ds)

# nakon pripreme podataka mozemo da zapocnemo pravljenje modela

# kreiranje trening i test dataseta
library(caret)
i <- createDataPartition(ds$High.Heating.Load, p = 0.8, list = F)
train <- ds[i,]
test <- ds[-i,]

summary(ds$High.Heating.Load)
summary(test)

# kreiranje modela
library(class)
set.seed(10)
model1.pred <- knn(train = train[,-9],
              test = test[,-9],
              cl = train$High.Heating.Load,
              k = 3)

# racunanje evaluacionih metrika
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

cm1 <- table(actual = test$High.Heating.Load, predicted = model1.pred)
cm1

eval1 <- compute.eval.metrics(cm1)
eval1

# sada gledamo da li mozemo da poboljsamo ovaj model ya neko drugo k
# pomocu krosvalidacije
library(e1071)
folds <- trainControl(method = 'cv',
                      number = 10)
kGrid <- expand.grid(.k = seq(from = 3, to = 23, by = 2))
kGrid

# poziv krosvalidacije
train(x = train[,-9],
      y = train$High.Heating.Load,
      method = "knn")
# krosvalidacija kaze da je optimalno k = 9
# pravimo novi model
set.seed(10)
model2 <- knn(train = train[,-9],
                   test = test[,-9],
                   cl = train$High.Heating.Load,
                   k = 9)

cm2 <- table(actual = test$High.Heating.Load, predicted = model2)
cm2
cm1

eval2 <- compute.eval.metrics(cm2)
eval2

# dobio sam losiji model sa boljim k....
print(data.frame(rbind(eval1, eval2), row.names = c('model 1 -', 'model 2 -')))












