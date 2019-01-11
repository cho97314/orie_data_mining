install.packages("glmnet")
library(glmnet)
BM1 <- Basic_Materials_Multiples[rowSums(is.na(Basic_Materials_Multiples))==0,]
BM <-scale(BM1)
BMdata <- data.frame(BM)
x = model.matrix(price~.,BMdata)
y = BMdata$price

grid <- 10^seq(10, -2, length=100)

####################Lasso model
lasso.mod <- glmnet(x, y, alpha=1, lambda=grid)
dim(coef(lasso.mod))
lasso.mod

#Split train and test data
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

#use k-fold to find best lambda
cv.out=cv.glmnet(x[train,], y[train], nfolds = 10, lambda = lasso.mod$lambda, alpha=1)
bestlam <- cv.out$lambda.min
bestlam

#graphs
plot(cv.out)
plot(cv.out$glmnet.fit, "lambda", label=TRUE)

#checking the coeff with best lambda
lasso.coef  <- predict(lasso.mod, type = 'coefficients', s = bestlam)[1:6,]
lasso.coef

####################Ridge model
ridge.mod <- glmnet(x, y, alpha=0, lambda=grid)
dim(coef(ridge.mod))
ridge.mod

#use k-fold to find best lambda
cv.out=cv.glmnet(x[train,], y[train], nfolds = 10, lambda = ridge.mod$lambda, alpha=0)
bestlam <- cv.out$lambda.min
bestlam

#graphs
plot(cv.out)
plot(cv.out$glmnet.fit, "lambda", label=TRUE)

#checking the coeff with best lambda
ridge.coef  <- predict(ridge.mod, type = 'coefficients', s = bestlam)[1:10,]
bestlam <- cv.out$lambda.min
ridge.coef
ridge.mod$beta[bestlam]



