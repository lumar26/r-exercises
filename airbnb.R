###########################################
### Rok AirBNB data
###########################################

initial.ds <- read.csv('data/airbnb.csv')
str(initial.ds)
summary(initial.ds)

# na kojim indeskima se nalazi NA vrednost za rewie_scores rating
na.indices <- which(is.na(initial.ds$review_scores_rating))
length(na.indices)

ds <- initial.ds[-na.indices, ]
str(ds)

# izdvajanje samo onih oopservacija sa vise od 11 stavki u ammenities koloni

str(ds$amenities)
class(ds[1,]$amenities)

# treba prvo da se pretvori u karakter varijablu
ds$amenities <- as.character(ds$amenities)

?strsplit
splitted <- strsplit(ds[1,]$amenities, ',')
splitted

as.vector(splitted)

length(unlist(splitted))

length(unlist(strsplit(ds$amenities, ',')))

# dodajemo dodatnu kolonu koja je pomocna dok ne podelimo dataset
ds$enough.amenities <- lapply(ds$amenities,  function(x) {
  l <- length(unlist(strsplit(x, ',')))
  ifelse(l>11, T, F)
})

enough.amen.ind <- which(ds$enough.amenities == T)
enough.amen.ind

#izbacujemo nepotrebnu kolonu i opservacije koje ne zadovoljavaju uslov

ds <- ds[enough.amen.ind, ]
ds$enough.amenities <- NULL

str(ds)

summary(ds)

# zahtev 2
# prikaz broja nedostajucih vrednosti ya svau

na.count <- apply(ds, 2, function(x) {
  length(which(is.na(x)))
})

na.count
# varijable bathrooms, beds and bedrooms imaju nedostajuce vrednosti. sve te varijable su numericke

# provera da nema negativnih vrednosti
negative.count <- apply(ds[,5:7], 2, function(x) {
  length(which(x < 0))
})
negative.count
# nema negativnih za ove varijable

#Prvo proveravam raspodelu ovih varijabli, ukoliko je noramalna NA menjam sa MEan, a aukoliko nije normalna NA menjam sa Median

normality.tests <- apply(ds[,5:7], 2, shapiro.test)
normality.tests
# p-value ya svaku varijablu je njae od 0.05 tako da nulta hipoteya o noarmalnoj raspodeli nije prihvacena
# menjamo medijanom

bath.med <- median(ds$bathrooms, na.rm = T)
bedr.med <- median(ds$bedrooms, na.rm = T)
beds.med <- median(ds$beds, na.rm = T)

# menjamo medijanama nedostajuce vrednosti
ds[which(is.na(ds$beds)), ]$beds <- beds.med
ds[which(is.na(ds$bedrooms)), ]$bedrooms <- bedr.med
ds[which(is.na(ds$bathrooms)), ]$bathrooms <- bath.med


# vise nema nedostajucih vrednosti u datasetu
summary(ds)

# treci zahtev
# kreirati varijablu expensive
summary(ds$price)
str(ds$price)
# cena je faktorska varijabla pa je treba pretvoriti u karakternu, skinuti znak $ i nakon toga pretvoriti u numericku
?substr
substr('$250.000', 2, 100000)

char.prices <- as.character(ds$price)
char.prices
char.prices <- unlist(lapply(char.prices, function(price) {substr(price, 2, 100000)}))

char.prices[715] <- "1200"

numeric.prices <- as.numeric(char.prices)
numeric.prices

# postoji jedna na vrednost za cenu, treba je zameniti na isti nacin kao i kod prethodnih varijabli
str(ds)

ds$price <- numeric.prices
summary(ds$price)


shapiro.test(ds$price)


ds$expensive <- ifelse(ds$price > median(ds$price), 'yes', 'no')
ds$expensive <- as.factor(ds$expensive)

# kNN modeliranje

# posto knn radi sa numerickim vrednostima moramo da uklonimo nepotrebne varijable iz modela
# amenities je varijabla koja moze da ima uticaj na model iako je karakterna tako da cemo na osnovu nje kreirati novu var
# koja pokazuje koliko broj dostupnih 'sadrzaja'
ds$amenities.count <- unlist(lapply(ds$amenities, function(x) {length(unlist(strsplit(x, ',')))}))
ds$amenities.count

str(ds)
summary(ds$property_type)
# uklanjamo promenljive id i name jer nemaju uticaja, kao  i amenities na osnovu koje smo napravili drugu varijablu

old.ds <- ds
summary(old.ds)
ds <- old.ds[,c(4,5,6,7,9,10,11,12,13,14)]
str(ds)
ds$expensive <- as.factor(ds$expensive)


# da li ima outiler-a
boxplot.stats(ds$accommodates)

outliers <- apply(ds[,-9], 2, function(x) {
  length(boxplot.stats(x)$out)
})
outliers
# svi imaju autlajere tako da koristimo standardizaciju
# potrebno je standardizovati sve vrednosti

#provera raspodela numerickih varijabli

summary(ds)

library(e1071)
?scale

shapiro.test(ds$bedrooms)
summary(ds$bedrooms)
scale(ds$bedrooms, center = median(ds$bedrooms), scale = IQR(ds$bedrooms))

#bedrooms ima istu 1 i 3 kvartilu sto bi trebalo da je nemoguce, tako da moramo to da izostavimo za sad
ds$bedrooms <- NULL

ds.std <- as.data.frame(apply(ds[,-8], 2, function(x) {
  scale(x, center = median(x), scale = IQR(x))
}))

summary(ds.std)
summary(ds.std$minimum_nights)
summary(ds$minimum_nights)

# potrebno je izbaciti i varijablu price jer smo na osnovu nje generisali izlaznu, kao i varijablu minimum_nights jer ima 
# preveliki opseg vrednosti i dominirace u modelu, a i svakako je ocigledno da vise noci donosi vecu cenu pa 
# necemo zalkjuciti nista sto nije ocigledno i dobicemo los model
ds$minimum_nights <- NULL
ds$price <- NULL

# pripremljen dataset za knn

# kreiranje train i test dataseta
library(caret)
set.seed(123)
ind <- createDataPartition(ds$expensive, p = 0.8, list = F)
train.data <- ds[ind,]
test.data <- ds[-ind,]


library(class)
#treniranje modela

model1 <- knn(train = train.data[,-6], test = test.data[,-6], 
              cl = train.data$expensive, k = 3) 
model1

# krosvalidacija za odredjivanje optimalnog broja suseda
folds <- trainControl(method = 'cv', number = 10)
kGrid <- expand.grid(.k = seq(from = 1, to = 31, by = 2))

train(train.data[,-6],
      train.data$expensive,
      method = 'knn', 
      trControl = folds,
      tuneGrid = kGrid)
# krosvalidacijom je utvrdjeno da je optimalan broj suseda koje treba posmatrati jednak 13




# izracunavanje evaluacionih metrika
compute.evals = function(cm) {
  tp <- cm[2,2] # s obzirom da nam je 'yes' pozitivna vrednost
  tn <- cm[1,1]
  fp <- cm[1,2]
  fn <- cm[2,1]
  
  a <- sum(diag(cm)) / sum(cm)
  p <- tp / (tp + fp)
  r <- tp / (tp + fn)
  f1 <- 2*r*p/(r+p)
  c(accuracy = a, precision = p, recal = r, F1 = f1)
}

cm1 <- table(actual = test.data$expensive, predicted = model1)
cm1
k3.evals <- compute.evals(cm1)


######### kopirano

model2 <- knn(train = train.data[,-6], test = test.data[,-6], 
              cl = train.data$expensive, k = 13) 
model2


cm2 <- table(actual = test.data$expensive, predicted = model2)
cm2
k9.evals <- compute.evals(cm2)


data.frame(rbind(k3.evals, k9.evals), row.names = c('k - 3 --> ', 'k - 9 --> '))






