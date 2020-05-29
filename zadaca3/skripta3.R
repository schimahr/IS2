library(ISLR)
library(gbm)
library(Metrics)
library(pdp)
library(caret)

# Varijabla podatci sadržava varijable iz dataseta Auto
podatci <- Auto

# Priprema setova
set.seed(15)
train <- sample(1:nrow(podatci), round(0.60 * nrow(podatci)))
set_treniranje <- podatci[train,]
set_testiranje <- podatci[-train,]

# Kreiranje modela
# Potrebne varijable: cylinders, weight, year, displacement, acceleration
set.seed(15)
model.gbm1 = gbm(mpg~cylinders+weight+year+displacement+acceleration, data=set_treniranje, distribution="gaussian", n.trees=5000, interaction.depth=4)
summary(model.gbm1)
# Predikcija
predikcija1 = predict(model.gbm1, set_testiranje, n.trees=5000, type="response")
vr1 = mean((predikcija1-set_testiranje$mpg)^2)
print(vr1)

# Graf parcijalne zavisnosti
require(gridExtra)
var1 <- plot(model.gbm1, i="weight")
var2 <- plot(model.gbm1, i="displacement")
grid.arrange(var1,var2, ncol=2)

# Poboljšani model
model.gbm2 = gbm(mpg~cylinders+weight+year+displacement+acceleration, data=set_treniranje, distribution="gaussian", n.trees=5000, interaction.depth=4, shrinkage = 0.01)
summary(model.gbm2)

predikcija2 = predict(model.gbm2, set_testiranje, n.trees=5000)
vr2 = mean((predikcija2-set_testiranje$mpg)^2)
print(vr2)
  
  
  
