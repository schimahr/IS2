library(readr)
edukacija <- read_csv("unishit/IS2/zadaca2/edukacija88.csv")


# izbacivanje varijable quizzes i ID
edukacija$id <- NULL
edukacija$quizzes <- NULL

# diskretizacija varijabli lectures, labs, videos, selfassesm
library(arules)
edukacija[,1] <- discretize(edukacija$lectures, method = "interval", breaks = 2, labels = c("SUFFICIENT","INSUFFICIENT"))
edukacija[,2] <- discretize(edukacija$labs, method = "interval", breaks = 2, labels = c("SUFFICIENT","INSUFFICIENT"))
edukacija[,3] <- discretize(edukacija$videos, method = "interval", breaks = 2, labels = c("SUFFICIENT","INSUFFICIENT"))
edukacija[,4] <- discretize(edukacija$selfassesm, method = "interval", breaks = 2, labels = c("SUFFICIENT","INSUFFICIENT"))

# stabla odluke i analiza
library(ISLR)
library(tree)
edukacija$grade <- as.factor(edukacija$grade)
set.seed(100)
drvce <- tree(edukacija$grade~.-edukacija$grade, data=edukacija)
#plot(drvce)
#text(drvce, pretty=0)

# priprema seta za treniranje i testiranje
setTreniranje = sample(1:nrow(edukacija),50)
setTestiranje = edukacija[-setTreniranje,]
setTestiranje.ocjena = edukacija$grade[-setTreniranje]

# stablo odluke za subset treniranje
set.seed(100)
drvce1 <- tree(edukacija$grade~.-edukacija$grade, data=edukacija, subset = setTreniranje)
#plot(drvce1)
#text(drvce1, pretty=0)

# predikcija na temelju stabla odluke
drvce.predikcija <- predict(drvce, setTestiranje, type="class")
rezultat <- table(drvce.predikcija,setTestiranje.ocjena)

# unakrsna validacija s ciljem određivanja optimalne razine složenosti
set.seed(100)
drvce.cv <- cv.tree(drvce,FUN = prune.misclass)

# testiranje za stablo sa subsetom
drvce1.cv <- cv.tree(drvce1, FUN = prune.misclass)

# orezivanje s 5,7 i 9 čvorova budući da tada ima najnižu devijancu
set.seed(100)
drvce.prune <- prune.misclass(drvce, best=5)
plot(drvce.prune)
text(drvce.prune, pretty = 0)

set.seed(100)
drvce.prune1 <- prune.misclass(drvce, best=7)
plot(drvce.prune1)
text(drvce.prune1, pretty = 0)

set.seed(100)
drvce.prune2 <- prune.misclass(drvce, best=9)
plot(drvce.prune2)
text(drvce.prune2, pretty = 0)

# predikcija na temelju novog stabla s orezivanjem
drvce.predikcija.new <- predict(drvce.prune, setTestiranje, type="class")
rezultat1 <- table(drvce.predikcija.new, setTestiranje.ocjena)

drvce.predikcija.new1 <- predict(drvce.prune1, setTestiranje, type="class")
rezultat2 <- table(drvce.predikcija.new1, setTestiranje.ocjena)

drvce.predikcija.new2 <- predict(drvce.prune2, setTestiranje, type="class")
rezultat3 <- table(drvce.predikcija.new2, setTestiranje.ocjena)

# testiranje za stablo sa subsetom
predikcija.testiranje <- predict(drvce1.prune, setTestiranje, type="class")
testiranje <- table(prediktor, setTestiranje.ocjena)



