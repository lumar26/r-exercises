initial.ds <- read.csv('data/processors.csv')
str(initial.ds)
names(initial.ds) <- c('adviser', 'name', 'syct'
                       , 'mmin', 'mmax', 'cache'
                       , 'chmin', 'chmax', 'perf', 'estperf')

names(initial.ds)

summary(initial.ds)
str(initial.ds)
complete.cases(initial.ds)
# nema nedostajucih vrednosti u ovom datasetu


# pravljenje podseta u kome je kes memorija ispod 100kB
low.cache.ind <- which(initial.ds$cache < 100)
low.cache.ind
ds <- initial.ds[low.cache.ind,]

#prve dve faktorske varijable: proizvodjac i ime nemaju nikakvog uticaja na modelovanje tako da njih uklanjamo
ds <- ds[,3:10]

summary(ds)

# varijabla koju predvidjamo je perf

library(caret)

corr.m <- cor(ds)
corr.m
library(corrplot)
corrplot.mixed(corr.m,  tl.cex = 0.75, number.cex = 0.75)

# koje su varijable u visokoj korelaciji sa perf
which(corr.m[7,-7] > 0.8 | corr.m[7,-7] < -0.8)
# mmmin, mmax i estperf imaju visoku kolerisanost sa perf varijablom, tako da 'emo te 3 arijable ukljuciti u model


#kreiranje trening i test particija
set.seed(5)
inds <- createDataPartition(ds$perf, p = 0.8, list = F)
train.data <- ds[inds,]
test.data <- ds[-inds,]

plot(train.data$perf)
plot(test.data$perf)


# kreiranje modela
model1 <- lm(perf ~ mmin + mmax + estperf + cache,
             data = train.data)
summary(model1)

# ispitivanje multikolinearnosti

library(car)
vif(model1)
sort(sqrt(vif(model1)))

# varijable koje imaju koren iz vif veci od 2 su problematicne
# najpre cemo iz modela ukloniti estpref
model2 <- lm(perf ~ mmin + mmax + cache,
             data = train.data)
summary(model2)

# prikaz evaluacionih metrika modela
par(mfrow=c(1,1))
plot(model2)

plot(model2$residuals)
# u modelu model2 primecujemo da reziduali ne odstupaju previse od stvarnih vrednosti sem u jednom ekstremnom slucaju
# tako da je linearnosd delimicno zadovoljena
# Reziduali tako]e imaju skoro pa normalnu raspodelu na osnovu Normal Q-Q dijagrama
# dijagram Scale-Location nam govori da je varijansa reziduala poprilicno ujednacen sem kod jedne ekstremne vrednosti koja utice na 
# na model 
# Na osnovu dijagrama Residuals vs Leverage uocavamo da postoje opservacije sa ekstremnim vrednostima, jedna u ovom slucaju

# kao zakljucak mozemo reci da model2 moze biti koristan za predvidjanje jer je linearnost dovoljno ispunjena kao i 
# normalnost raspodele reziduala, kao i ujednacensot varijabiliteta reziduala

sort(sqrt(vif(model2)))
# nakon uklanjanja estperf ne postoji vise problem multikolinearnosti i u modelu uocavamo da su sve odabrane varijable statisticki 
# veoma znacajne

# interpretacija modela MODEL2

library(ggplot2)
plot(model2$residuals)

summary(model2$residuals)
# možemo uočiti da je u modelu odsek na y osi jednak 23.01
# koeficijent za prediktor mmin je jednak 0.009659 sto znvi da ukoliko se minimalna kolicina memorije poveca za jedan posto onda ce se obj perf na bm testu povecati za 0.9 posto
# ...
# R-squared indiator nam govori da je ovim modelom obuhvaceno 83.58 posto ukupnog varijabiliteta
# F statistika nam govori da model ima prediktivnu moc jer je odbijena nulta hippoteza da prediktivna moc ne postoji na osnovu p-value


# predikcije
model2.predictions <- predict(model2, test.data, interval = 'predict')
class(model2.predictions)

test.data <- cbind(test.data, pred = as.data.frame(model2.predictions)$fit)
test.data$pred.upr <- NULL



ggplot(test.data) +
  geom_density(mapping = aes(x = perf, color = 'real')) + 
  geom_density(mapping = aes(x = pred, color = 'predicted')) + 
  theme_classic()
# na osnovu grafika vidimo da se model2 veoma lose pokazao na test podacima i da se ne moze uzeti kao pouzdan 


RSS <- sum((test.data$perf - test.data$pred)^2)
RSS

TSS <- sum ((mean(train.data$perf) - test.data$perf)^2)
TSS

R.sq <- 1 - RSS/TSS 
R.sq
# pokrivenost varijabiliteta je prilicno visoka i iznosi 89.4% sto je zadovoljavajuce

RMSE <- sqrt(RSS/nrow(test.data))
RMSE
summary(initial.ds$perf)
# s obzirom na raspodelu vrednosti promenljive koja opisuje performanse mozemo uociti da je greska od 40 prilicno velika




