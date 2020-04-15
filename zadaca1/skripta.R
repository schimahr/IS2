library(readr)
edukacija <- read_csv("unishit/IS2/zadaca1/edukacija90.csv")

# funkcija integer_breaks s kojom dobivamo integer vrijednosti na Y osi
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

# funkcija ggplotConfusionMatrix za argument prima model konfuzijske matrice za koji kreira plot
ggplotConfusionMatrix <- function(m){
  mytitle <- paste("Accuracy", percent_format()(m$overall[1]),
                   "Kappa", percent_format()(m$overall[2]))
  p <-
    ggplot(data = as.data.frame(m$table) ,
           aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
    theme(legend.position = "none") +
    ggtitle(mytitle)
  return(p)
}

library(tidyverse)

# a) Utjecaj varijable quizzes na ocjenu

pA <- ggplot(data = edukacija, aes(edukacija$quizzes,edukacija$grade,colour=edukacija$grade))
pA <- p + geom_point()
pA <- p + ggtitle("Odnos bodova ostvarenih na kvizu na ocjenu")
pA <- p + labs(y="Ocjena", x="Bodovi na kvizu", colour = "Ocjena")
print(p)

# b) Utjecaj varijable quizzes u kombinaciji s ostalim varijablama

# quizzes i lectures
pB1 <- ggplot(data = edukacija, aes(edukacija$quizzes,edukacija$lectures,colour=edukacija$grade))
pB1 <- pB1 + geom_point()
pB1 <- pB1 + scale_y_continuous(breaks = integer_breaks())
pB1 <- pB1 + ggtitle("Utjecaj varijable quizzes u kombinaciji s varijablom lectures")
pB1 <- pB1 + labs(y="Broj bodova ostvarenih na predavanjima",x="Bodovi na kvizu", colour="Ocjena")
print(pB1)

# quizzes i vidoes
pB2 <- ggplot(data=edukacija, aes(edukacija$quizzes, edukacija$videos, colour=edukacija$grade))
pB2 <- pB2 + geom_point()
pB2 <- pB2 + scale_y_continuous(breaks = integer_breaks())
pB2 <- pB2 + ggtitle("Utjecaj varijable quizzes u kombinaciji s varijablom videos")
pB2 <- pB2 + labs(y="Broj pokretanja video materijala", x="Bodovi na kvizu", colour="Ocjena")
print(pB2)

# quizzes i labs
pB3 <- ggplot(data=edukacija, aes(edukacija$quizzes, edukacija$labs, colour=edukacija$grade))
pB3 <- pB3 + geom_point()
pB3 <- pB3 + scale_y_continuous(breaks = integer_breaks())
pB3 <- pB3 + ggtitle("Utjecaj varijable quizzes u kombinaciji s varijablom labs")
pB3 <- pB3 + labs(y="Broj bodova ostvarenih na labosima", x="Bodovi na kvizu", colour="Ocjena")
print(pB3)

# quizes i selfassesm
pB4 <- ggplot(data=edukacija, aes(edukacija$quizzes, edukacija$selfassesm, colour=edukacija$grade))
pB4 <- pB4 + geom_point()
pB4 <- pB4 + scale_y_continuous(breaks = integer_breaks())
pB4 <- pB4 + ggtitle("Utjecaj varijable quizzes u kombinaciji s varijablom selfassesm")
pB4 <- pB4 + labs(y="Broj klikova u okviru samoprovjere", x="Bodovi na kvizu", colour="Ocjena")
print(pB4)

# Pokušaj izrade mosaic plot-a, ali ggplot2 3.3.0 je potrgao ggmosaic biblioteku i čeka se update.
library(ggmosaic)

# quizzes i slog
mosaicSlog <- ggplot(data=edukacija)
mosaicSlog <- mosaicSlog + geom_mosaic(aes(x=product(quizzes, slog), fill=grade))
mosaicSlog <- mosaicSlog + labs(x="Pristupio ili ne", title="Utjecaj varijable quizzes u kombinaciji s varijablom slog")
print(mosaicSlog)

pB5 <- ggplot(data=edukacija, aes(edukacija$quizzes, edukacija$slog, colour=edukacija$grade))
pB5 <- pB5 + geom_point()
pB5 <- pB5 + scale_y_continuous(breaks = integer_breaks())
pB5 <- pB5 + ggtitle("Utjecaj varijable quizzes u kombinaciji s varijablom slog")
pB5 <- pB5 + labs(y="Pristupio (1) ili ne (0)", x="Bodovi na kvizu", colour="Ocjena")
print(pB5)

#quizzes i forum
mosaicForum <- ggplot(data=edukacija)
mosaicForum <- mosaicForum + geom_mosaic(aes(x=product(quizzes, forum), fill=grade))
mosaicForum <- mosaicForum + labs(x="Pristupio ili ne", title="Utjecaj varijable quizzes u kombinaciji s varijablom forum")
print(mosaicForum)

pB6 <- ggplot(data=edukacija, aes(edukacija$quizzes, edukacija$forum, colour=edukacija$grade))
pB6 <- pB6 + geom_point()
pB6 <- pB6 + scale_y_continuous(breaks = integer_breaks())
pB6 <- pB6 + ggtitle("Utjecaj varijable quizzes u kombinaciji s varijablom forum")
pB6 <- pB6 + labs(y="Pristupio (1) ili ne (0)", x="Bodovi na kvizu", colour="Ocjena")
print(pB6)

# quizzes i red
mosaicRed <- ggplot(data=edukacija)
mosaicRed <- mosaicRed + geom_mosaic(aes(x=product(quizzes, red), fill=grade))
mosaicRed <- mosaicRed + labs(x="Pristupio ili ne", title="Utjecaj varijable quizzes u kombinaciji s varijablom red")
print(mosaicRed)

pB7 <- ggplot(data=edukacija, aes(edukacija$quizzes, edukacija$red, colour=edukacija$grade))
pB7 <- pB7 + geom_point()
pB7 <- pB7 + scale_y_continuous(breaks = integer_breaks())
pB7 <- pB7 + ggtitle("Utjecaj varijable quizzes u kombinaciji s varijablom red")
pB7 <- pB7 + labs(y="Pristupio (1) ili ne (0)", x="Bodovi na kvizu", colour="Ocjena")
print(pB7)

# c) 
# Učitavanje potrebnih biblioteka

library(ISLR)
library(data.table)
library(plyr)
library(stringr)
library(ROCR)
library(Metrics)
library(yardstick)
library(caret)
library(e1071)
library(scales)

# Priprema podataka i skupova za treniranje, testiranje i ucenje

skupTraining <- edukacija[1:50,]
skupTest <- edukacija[51:77,]
skupPredikcija <- edukacija$grade[51:77]
skupPredikcija <- replace(skupPredikcija, skupPredikcija==c("GOOD"),c("PASS"))

# Model 1 - varijable: quizzes, lectures

model1 <- glm(formula = factor(grade) ~ quizzes + lectures, family = "binomial", data=skupTraining)
stats1 <- summary(model1)

predikcija1 <- predict(model1, skupTest, type="response")
predikcija1 <- ifelse(predikcija1>0.5, "PASS", "FAIL")

cfm1 <- confusionMatrix(factor(predikcija1),factor(skupPredikcija))
print(cfm1)

print(ggplotConfusionMatrix(cfm1))

# Model 2 - varijable: quizzes, labs

model2 <- glm(formula = factor(grade) ~ quizzes + labs, family = "binomial", data=skupTraining)
stats2 <- summary(model2)

predikcija2 <- predict(model2, skupTest, type="response")
predikcija2 <- ifelse(predikcija2>0.5, "PASS", "FAIL")

cfm2 <- confusionMatrix(factor(predikcija2),factor(skupPredikcija))
print(cfm2)

print(ggplotConfusionMatrix(cfm2))

# Model 3 - varijable: quizzes, videos, selfassesm

model3 <- glm(formula = factor(grade) ~ quizzes + videos + selfassesm, family = "binomial", data=skupTraining)
stats3 <- summary(model3)

predikcija3 <- predict(model3, skupTest, type="response")
predikcija3 <- ifelse(predikcija3>0.5, "PASS", "FAIL")

cfm3 <- confusionMatrix(factor(predikcija3),factor(skupPredikcija))
print(cfm3)

print(ggplotConfusionMatrix(cfm3))

# Model 4 - varijable: quizzes, slog, red, forum, kruzna, dinamicko, stabla1, stabla2, demons

model4 <- glm(formula = factor(grade) ~ quizzes + slog + red + forum + kruzna + dinamicko + stabla1 + stabla2 + demons, family = "binomial", data=skupTraining)
stats4 <- summary(model4)

predikcija4 <- predict(model4, skupTest, type="response")
predikcija4 <- ifelse(predikcija4>0.5, "PASS", "FAIL")

cfm4 <- confusionMatrix(factor(predikcija4),factor(skupPredikcija))
print(cfm4)

print(ggplotConfusionMatrix(cfm4))

# Model 5 - varijable: sve

model5 <- glm(formula = factor(grade) ~ quizzes + lectures + videos + selfassesm + + labs + slog + red + forum + kruzna + dinamicko + stabla1 + stabla2 + demons, family = "binomial", data=skupTraining)
stats5 <- summary(model5)

predikcija5 <- predict(model5, skupTest, type="response")
predikcija5 <- ifelse(predikcija5>0.5, "PASS", "FAIL")

cfm5 <- confusionMatrix(factor(predikcija5),factor(skupPredikcija))
print(cfm5)

print(ggplotConfusionMatrix(cfm5))
