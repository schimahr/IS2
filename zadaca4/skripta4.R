library(ISLR)
library(glmnet)
library(readr)
library(tidyverse)
library(data.table)

# 1. zadatak

student <- fread("http://www.odraz.com/student-por.csv")

x <- model.matrix(G3~., student)[,-1]
y <- student$G3

# 2. zadatak

ridge.model = glmnet(x,y,alpha=0, lambda=10^seq(10,-2))
plot(ridge.model)

dim(coef(ridge.model))

# 3. zadatak - podjela skupa

set.seed(123)
train <- sample(1:nrow(student), nrow(x)/2)
test=(-train)
set_testiranje=y[test]

# 4. zadatak - cv i lambda

cv.model = cv.glmnet(x[train,],y[train], alpha=0)
plot(cv.model)

najboljaLambda = cv.model$lambda.min

ridge.predikcija = predict(ridge.model, s=najboljaLambda, newx = x[test,])
cat("MSE pogreska: ", mean((ridge.predikcija-set_testiranje)^2))

# 5. zadatak

model.treniranje = glmnet(x,y,alpha=0)
model.predikcija = predict(model.treniranje, type="coefficients", s=najboljaLambda)
# pregled koeficijenata
View(model.predikcija[1:42,])

# 6. zadatak - lasso
lasso.model = glmnet(x[train,], y[train], alpha=1, lambda=10^seq(10,-2))
plot(lasso.model)

set.seed(123)
cv.lasso = cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.lasso)

najboljaLambda1 = cv.lasso$lambda.min
lasso.predikcija = predict(lasso.model, s=najboljaLambda1,newx=x[test,])
cat("MSE pogreska: ", mean(lasso.predikcija-set_testiranje)^2)


model.treniranje1 = glmnet(x,y,alpha=1, lambda=10^seq(10,-2))
lasso.koeficijenti = predict(model.treniranje1, type="coefficients", s=najboljaLambda1)
# pregled koeficijenata
View(lasso.koeficijenti[1:42,])


