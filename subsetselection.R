install.packages("ISLR")
library(ISLR)
install.packages("leaps")
library(leaps)

tech <- tech_multiples[rowSums(is.na(tech_multiples)) == 0,]
techdata  <- data.frame(tech)
tech.fit <- lm(formula = price~.,techdata)
summary(tech.fit)

regfit.full <- regsubsets(price~.,techdata,nvmax=3)
reg.summary <- summary(regfit.full)
reg.summary
