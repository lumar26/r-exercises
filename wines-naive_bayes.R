all.wines <- read.csv('data/winemag-data-130k-v2.csv', stringsAsFactors = F)

str(all.wines)
summary(all.wines)

as.factor(all.wines$country)

length(which(all.wines$country == 'Argentina' | all.wines$country == 'France' | all.wines$country == 'Italy'))

wines <- all.wines[which(all.wines$country == 'Argentina' | all.wines$country == 'France' | all.wines$country == 'Italy'), ]

summary(wines)

apply(wines[, -(5 + 6)], 2, function(x){
  length(which(x == '-' | x == ''))
})
# samo promenljive region_1 i region_2 i taster_name i designation imaju nedostajuce vrednosti, od onih varijabli koje nisu numericke
# za promenljivu designation bi bilo prikladno da se na mesta gde postoje nedostajuce vrenosti unese vrednos 'No wineyard' jer bi cinjenica da vinarija ima svoj vinograd mogla da utice na konacnu cenu

wines[which(wines$designation == '-' | wines$designation == ''), 'designation'] <- 'No wineyard'
# region_2 promanljiva ima sve nedostajuce vrednosti tako da ne moze biti od koristi u kreiranju modela
wines$region_2 <- NULL
# region moze da bude od znacaja u pravljenju modela, medjutim nelogicno je popuniti nedostajuce vrednosti najcescom vrednsoti u celom datasetu

wines$country <- as.factor(wines$country)
wines$province <- as.factor(wines$province)
summary(wines)

# za svaku provinciju odredjujemo najcescu regiju ---- mnogo mukotrpno, nema poente
wines$region_1[which(wines$region_1 == '' | wines$region_1 == '-')] <- 'No region'

wines$region_1 <- as.factor(wines$region_1)

# pregled numerickih varijabli
# varijabla price sadrzi nedostajuce vrednosti, prvo proveravamo raspodelu

na.prices <- which(is.na(wines$price))
na.percent <- length(na.prices) / length(wines$price)
# 15% vrednosti je NA, probacemo da eliminisemo nedostajuce vrednosti
shapiro.test(wines$price[-na.prices])

# preveliki uzorak za sapirov test, kotisticu Anderson-Darling test
install.packages('nortest')
library(nortest)
ad.test(wines$price[-na.prices])
# ovaj test kaze da rqaspodela nije noramlna tako da nedostajuce vrednosti menjamo medijanom
wines$price[na.prices] <- median(wines$price, na.rm = T)

summary(wines)

first.q <- quantile(wines$price, 0.25)
# kreiramo izlaznu varijablu i uklanjamo onu na osnovu koje je generisana
wines$price.category <- ifelse(wines$price < first.q, 'cheap', 'not_cheap')
wines$price <- NULL


# kreiranje modela pomocu Naive Bayes metode
# odabir varijabli:
# za ovu metodu se koriste faktorske varijable i numericke sa noramlnom raspodelom
# proveraavamo najpre raspodelu promenljive points, ako je normalna onda ok, ako ne moramo da je transformisemo u faktorsku
ad.test(wines$points)
# nije noramlne raspodele, pretvaramo je u faktorsku 
summary(wines$points)
library(bnlearn)
disc.points <- discretize(as.data.frame(as.numeric(wines$points)), 
                          method = 'quantile',
                          breaks = c(4))
disc.points
class(wines$points)

ds <- cbind(wines, disc.points)
str(ds)
names(ds)[14] <- "points.disc"
ds$points <- NULL
str(ds)
# opis svakako ne moze da bude dobar prediktor pa ga izbacujemo
ds$description <- NULL
# somelieri mogu da budu od znacaja za predvidjanje ali nema potrebe zadrzati i twitter account, 
# naziv takodje ne moze da bude prediktor, i varijabla X koja predstavlja redni broj takodje ne treba da bude ukljucena

ds$X <- NULL

ds$taster_twitter_handle <- NULL
ds$title <- NULL

# pretvaranje karakternih varijabli u faktorske
ds$designation <- as.factor(ds$designation)
ds$winery <- as.factor(ds$winery)
ds$price.category <- as.factor(ds$price.category)
ds$taster_name <- as.factor(ds$taster_name)
ds$variety <- as.factor(ds$variety)

str(ds)
table(ds$taster_name, ds$price.category)

#taster ima nedostajuce vrednosti, i to cak 20 posto, tako da ne bi bilo pozeljno menjati te vrednosti najcescom jer
# bi onda ta vrednsot bila previse dominantna, a u slucaju someliea ne bi imalo smisla dodeliti jednoj osobi dodatnih 20% 'kusanja' vina
pct.na.tasters <- length(which(ds$taster_name == '')) / nrow(ds)
ds$taster_name <- as.factor(ds$taster_name)
table(ds$taster_name)

# ukllanjamo ovu varijablu
ds$taster_name <- NULL
summary(ds)

# takodje promenljiva koja predstavlja naziv vinarije nece vrv imati neku prediktivnu moc i samo ce opteretiti model
# ali probacemo sa njom prvo pa cemo je posle izbacit, da vidimo kako ide

str(ds)

# kreiranje modela

# odvajanje trening i test seta
library(caret)

set.seed(11)

ind <- createDataPartition(ds$price.category, p = 0.8, list = F)
train.ds <- ds[ind, ]
test.ds <- ds[-ind,]


#kreiranje modela
library(e1071)
nb1 <- naiveBayes(price.category ~ ., 
                  data = train.ds)
summary(nb1)
nb1

# predvidjanja
nb1.predict <- predict(nb1, test.ds, type = 'raw')
round(head(nb1.predict), 6)


nb1.predict.class <- predict(nb1, test.ds, type = 'class')
head(nb1.predict.class, 12)

# evaluacione metrike

# matrica konfuzije koja sadrzi odnos predvidjenih vrednosi i stvarnih vrednosti koje se nalaze u test datasetu
cm1 <- table(actual = test.ds$price.category, predicted = nb1.predict.class)
# odmah se vidi da je ovo veoma los model jer ima previse false negative vrednosi

compute.evals = function(cm){
  tp <- cm[2,2]
  tn <- cm[1,1]
  fp <- cm[1,2]
  fn <- cm[2,1]
  p <- tp / (tp + fp)
  r <- tp / (tp + fn)
  a <- sum(diag(cm)) / sum(cm)
  f1 <- 2*p*r/(p + r)
  c(accuracy = a, precision = p, recall = r, F1 = f1)
}

compute.evals(cm1)
# ali evaluacione metrike kazu da cak i nije lose toliko koliko se cinilo:
# odnos tacno predvidjenih u odnosu na test je oko 71%, sto nije bas sjajno
# odnos tacno predvidjenih pozitivnih vrednosti u odnosu na sve koje su predvidjene kao pozitivne je 90 posto sto je veoma solidno
# odnos tacno predvidjenih pozitivnih vrednosi (da je vino jefitino) u osnosu na stvarno jeftina vina iz test seta je 69% sto nije bas sjajno

# poboljsavanje modela pomocu ROC krive
library(pROC)

roc.params <- roc(response = test.ds$price.category,
                  predictor = nb1.predict[,1])

roc.params$auc
# kao indikator pogodnosi modela posmatra se povrsina ispod roc krive koja ovde izonsi 0.7992 sto je delimicno okej ali trebalo bi da
# bude dosta bolje, tj treba daziti sto vecoj toj povrsini

# zato cemo promeni ti treshold na optimalniju vrednost
plot.roc(roc.params,
         print.thres = T, 
         print.thres.best.method = 'youden')
# sa slike uocavamo da je najbolja vrednost za threshold 0.538 sto se vrlo malo razlikuje od one koje vec imamo a to je 0.5
# u tom slucaju je specificity = 0.761 a sensitivity 0.707

roc.coords.best<- coords(roc.params,
            ret = c("accuracy", "spec", "sens", "thr"), 
                                       x = 'best')
roc.coords.best

# kreiranje predikcija sa novim tresholdom
nb.pred.new <- ifelse(nb1.predict[,1] > roc.coords.best$threshold, yes = 'cheap', no = 'not_cheap')
  class(nb.pred.new)
  nb.pred.new <- as.factor(nb.pred.new)  
  
# nova matrica konfuzije
  cm2 <- table(actual = test.ds$price.category, predicted = nb.pred.new)
cm2  
data.frame(rbind(compute.evals(cm1), compute.evals(cm2)), row.names = c('thres = 0.5 --> ', 'thres = 0.5375791 -->'))

# pomeranjem threshold-a uspeli smo da poboljsamo za ninjansu sve metrike sem precision, tj. odnos
# predvidjenih i stvarno jeftinih vina u odnosu na sva koja smo predvideli da su jefitina...