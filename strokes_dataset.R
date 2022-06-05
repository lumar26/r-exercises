ds <- read.csv("healthcare-dataset-stroke-data.csv")
str(ds)

# sredjivanje podataka
summary(ds)
summary(ds$bmi)

# sredjivanje kolone bmi - izracunavanje sredne vrednosti ali bez na vrednosti, izbacivanje n/a vrednosti najpre zamena sa , 
bmi.not.na <- which(ds$bmi != "N/A")
head(bmi.not.na)

ds[-bmi.not.na, ]$bmi <- NA

# uzimanje faktorskih varijabli z bmi i pretvaranje u numericku kako bi mogla da se izmeri srednja vrednost 
# kad se odredi srednja vrednost (median ili mean) tada se NA vrednosti menjaju sa tom srednjom
numeric.bmi.values <- as.numeric(ds[bmi.not.na, ]$bmi)
head(numeric.bmi.values)
numeric.bmi.values <- sapply(numeric.bmi.values, FUN = function(x) {x/10}) # jer se pretvaranjem u numericku zanemaruje .
numeric.bmi.values


shapiro.test(numeric.bmi.values) # nije normalna raspodela pa koristimo medijanu

# prosecna vrednost kojom menjamo
avg.bmi <- median(numeric.bmi.values)
summary(numeric.bmi.values)
avg.bmi

# pretvaramo bmi u numericku
ds$bmi <- as.numeric(ds$bmi)
ds$bmi <- sapply(ds$bmi, FUN = function(x) {x/10})
str(ds)

# dodela srednje vrednosti
ds[-bmi.not.na, ]$bmi <- avg.bmi

# ispitivanje bmi varijable
summary(ds$bmi)

#pretvaranje u karakterne: hypertension i heart desease i stroke, i izbacivanje postojecih
ds$Stroke <- ifelse(test = ds$stroke ==1, "yes", "no")
ds$Hypertension <- ifelse(test = ds$hypertension ==1, "yes", "no")
ds$HeartDisease <- ifelse(test = ds$heart_disease ==1, "yes", "no")
str(ds)
ds$hypertension <- NULL
ds$heart_disease <- NULL
ds$stroke <- NULL

# konverzija u faktorsku varijablu
ds$Hypertension <- as.factor(ds$Hypertension)
ds$Stroke <- as.factor(ds$Stroke)
summary(ds$Stroke)
ds$HeartDisease <- as.factor(ds$HeartDisease)

##############################################################################################################

round(prop.table(table(ds$Stroke)), 3)


# kreiranje test i train dataseta
library(caret)
set.seed(10)
train.indices <- createDataPartition(ds$Hypertension, p = 0.8, list = FALSE)
length(train.indices)
train.data <- ds[train.indices,]
summary(train.data)
test.data <- ds[-train.indices,]
head(train.data)


# koje varijable koristim u predvidjanju
input.var.indices <- c(2:9)
ds[,input.var.indices]

# kreiranje modela na osnovu train dataseta
library(rpart)
library(rpart.plot)
set.seed(10)

model1 <- rpart(Hypertension ~ colnames(ds[,input.var.indices]), 
                data = train.data, 
                method = "class",
                control = rpart.control(minsplit = 20, cp = 0.004))
print(model1)
rpart.plot(model1, extra = 104)

# kreiranje predikcija modela
model1.predict <- predict(model1, test.data, "class")
model1.predict

# izracunavanje evaluacionih metrika
cm1 <- table(actual = test.data$Hypertension, predicted = model1.predict)
cm1

compute.eval.metrics <- function(cm) {
  tp <- cm[2,2]
  tn <- cm[1.1]
  fp <- cm[1,2]
  fn <- cm[2,1]
  a <- sum(diag(cm))/sum(cm)
  p <- tp / (tp + fp)
  r <- tp / (tp + fn)
  f1 <- 2*p*r/(p+r)
  c(accuracy = a, precision = p, recall = r, F1 = f1)
  }
eval1 <- compute.eval.metrics(cm1)
eval1

# krosvalidacija za vrednost cp
set.seed(10)
folds = trainControl(method = "cv", number = 10) 
cpGrid <- expand.grid(.cp = seq(from = 0.001, to = 0.2, by = 0.001))

train(Hypertension ~ .,
      data = train.data, 
      method = "rpart",
      control = rpart.control(minsplit = 20),
      trControl = folds,
      tuneGrid = cpGrid)
