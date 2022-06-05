library(ISLR)
??ISLR

?College
ds <- College
ds <- College[,c('Private', 'Enroll', 'Top10perc', 'Top25perc', 'F.Undergrad', 'P.Undergrad', 'Outstate',
                 'Room.Board', 'Books', 'Personal', 'perc.alumni', 'Expend', 'Grad.Rate')]

summary(ds)
str(ds)

# provera koliki je odnos izmedju prijavljenih studenata i primljenih
summary(ds$Apps - ds$Accept)
summary(ds$Accept - ds$Enroll)
?which

pairs(ds[,1:5])

third.quartile <- quantile(ds$Grad.Rate, 0.75)
ds$High.Grad.Rate <- ifelse(ds$Grad.Rate > third.quartile, "Yes", "No")

summary(ds$High.Grad.Rate)

#definisanje izlazne promenljive za nas model
ds$High.Grad.Rate <- as.factor(ds$High.Grad.Rate)

# uklanjanje promenljive na osnovu koje smo napravili izlaznu
ds$Grad.Rate <- NULL

str(ds[,-13])

# provera raspodele numerickih varijabli
apply(ds[,2:12],2, shapiro.test)


# na osnovu Sapiro Vilks testa ne postoji ni jedna varijabla sa normalnom raspodelom u datasetu tako da je poterbno za 
# sve promenljive izvrsiti diskretizaciju
library(bnlearn)
discretized.numerics <- discretize(ds[,2:12],
                                   method = "quantile",
                                   breaks = c(5,5,5,5,5,5,5,5,5,5,5))

str(discretized.numerics)

old.ds <- ds

# kreiranje dataseta sa svim faktorskim promenljivima nad kojim je moguce primeniti naive bayes
ds <- cbind(discretized.numerics, ds[,c(1,13)])
str(ds)

# kreiranje trening i test dataseta
library(caret)
set.seed(10)
indices <- createDataPartition(ds$High.Grad.Rate, p = 0.8, list = F)
train <- ds[indices,]
test <- ds[-indices,]

library(e1071)
model1 <- naiveBayes(High.Grad.Rate ~ ., 
                     data = train)
model1

nb.predict.1 <- predict(object = model1, 
                        newdata = test,
                        type = 'raw')
round(nb.predict.1, 3) # predikcije sa verovatnocama

nb.predict.1.classified <- predict(object = model1, 
                        newdata = test,
                        type = 'class')
summary(nb.predict.1.classified)

# nakon kreiranja modela sledi izracunavanje evaluacionih metrika

cm1 <- table(actual = test$High.Grad.Rate, predicted = nb.predict.1.classified)
cm1

# za pozitivnu vrednost uzimamo Yes, tj. vrednost da univerzitet ima visok broj onih koji su diplomirali 
compute.evals <- function(cm){
  tp <- cm[2,2]
  tn <- cm[1,1]
  fp <- cm[1,2]
  fn <- cm[2,1]
  a <- sum(diag(cm)) / sum(cm)
  p <- tp / (tp + fp)
  r <- tp / (tp + fn)
  f1 <- p*2*r/(p + r)
  c(accuracy = a, precision = p, recall = r, F1 = f1)
}

print(compute.evals(cm1))
# prvi dobijeni model nema zadovoljavajuce evaluacione metrike jer su tacnost, preciznost i odziv na niskom nivou, a
# pogotovo precisnost, tj. procenat univerziteta za koje smo tacno predvideli visok stepen diplomiranja, u odnosu na sve
# za koje smo to istop predvideli, je nizak i iznosi oko 50% uspesnosti

# potrebno je poboljsati model tako sto odaberemo bolji treshhold
# pomocu ROC krive

library(pROC)
roc.params <- roc(response = test$High.Grad.Rate,
                  predictor = nb.predict.1[, 2])
roc.params$auc
plot.roc(roc.params,
         print.thres = T, 
         print.thres.best.method = "youden")
roc.coords <- coords(roc.params,
                     ret = c("accuracy", "spec", "sens", "thr"),
                     x = 'local maximas')
round(roc.coords, 3)

# dodela drugih vrednosti predikcijama na osnovu novog treshold-a
round(nb.predict.1, 3)
new.prediction <- ifelse(nb.predict.1[,2] > 0.175, "Yes", "No")
cm2 <- table(actual = test$High.Grad.Rate, predicted = new.prediction)
cm2
print(compute.evals(cm1))
