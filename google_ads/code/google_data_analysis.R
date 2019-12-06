##Play aroung with ridge regression on google ads data
# clear memory
rm(list=ls())
library(data.table); library(ggplot2); library(glmnet); library(magrittr); library(tidyverse)

#read in data
dt <- fread("~/Desktop/google-political-ads-transparency-bundle/google-political-ads-creative-stats.csv")
dt.target <-fread("~/Desktop/google-political-ads-transparency-bundle/google-political-ads-campaign-targeting.csv")
dt[, startYear := substr(Date_Range_Start,1,4)]
dt[, regions := ifelse(grepl("EU", Regions), "EU", "US")]
mod <- lm(as.numeric(Spend_Range_Max_USD) ~ startYear + Impressions + Num_of_Days + Ad_Type + regions, data = dt)
summary(mod)

#############
#Exploratory
#############
summary(dt$Spend_Range_Max_USD)
dt[Spend_Range_Max_USD<4000, hist(Spend_Range_Max_USD)]

#Histogram continuous variables
ggplot(dt %>%
         filter(Num_of_Days<250), aes(log(Num_of_Days))) +
  geom_histogram()
dt.num <- select(dt, Num_of_Days, Spend_Range_Max_USD)
plot(dt.num$Num_of_days, dt.num$Spend_Range_Max_USD)

##gradient descent w linear regression-minimize the cost function
  #test out what coefs are w reg lm (uses maximum likelihood estimation)
  lm(Spend_Range_Max_USD ~ Num_of_Days, data=dt)
  x1 <- dt$Num_of_Days
  x2 <- dt$startYear
  y <- dt$Spend_Range_Max_USD
  
  ##gradient descent
  #initialize weights (coefs)
  x1_weight <- 0
  x2_weight <- 0
  con <- 0
  
  #set other parameters
  n <- length(x1)
  L <- 0.0001  # The learning Rate
  iters <- 100
  #partial differential of the cost function w respect to particular weight (startYear for example)

  for (i in 1:iters) {
    yhat <- x1*x1_weight + con #The current predicted value of Y
    D_x1 <- (-2/n) * sum(x1 * (y - yhat)) #Partial derivative wrt x1
    D_con = (-2/n) * sum(y - yhat) #Derivative wrt c
    x1_weight = x1_weight - L * D_x1  # Update x1
    con = con - L * D_con  # Update intercept
  }
  print(x1_weight, con)
#############
#ridge regression
dt <- dt[!is.na(Spend_Range_Max_USD)]
x <- model.matrix(as.numeric(Spend_Range_Max_USD) ~ startYear + Impressions + Num_of_Days + Ad_Type + regions, data = dt)
#y_noise <- rnorm(207890, sd=10)
y <- dt$Spend_Range_Max_USD
#y <- abs(y + y_noise)
grid <- 10^seq(10,-2, length =100)


ridge.mod <- glmnet(x, y, alpha=0, lambda=grid, standardize.response = F)
ridge.mod$lambda[50]
coef(ridge.mod)[,50]

ridge.mod$lambda[70]
coef(ridge.mod)[,70]

#Split into training and testing data
set.seed (1)
train <- sample(1:nrow(x), nrow(x)/2) %>% sort()
test <- setdiff(1:nrow(x), train)
y.test <- y[test]
cv.out <- cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min
ridge.pred <- predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2)
predict(ridge.mod, type = "coefficients", s=bestlam)
