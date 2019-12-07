library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(glmnet)
library(MASS)
library(gam)

setwd("/Users/yuetongliu/Desktop")
fltrain <- read_csv("./Project652/fltrain.csv.gz")

dim(fltrain)

## 1. Missing data

## character -> factor
fl <- fltrain
for(i in 1:ncol(fl)) {
  if(typeof(fl[[i]]) == "character") {
    fl[[i]] <- factor(fl[[i]])
  }
}

## count the percentage of missing values in each variable.
pct_miss <- function(x) { as.numeric(sum(is.na(x)))/200000 }
pct_miss_list <- sapply(fl,pct_miss)

## remove variables have more than 10% missing values
names(pct_miss_list[ pct_miss_list > 0.1])
fl1 <- fl%>% 
  dplyr::select(-year.y,-type,-manufacturer,-model,-engines,-seats, -speed, -engine,-wind_gust,-pressure)%>%
  na.omit()

## 2. Judgemental (remove & format)

str(fl1)

fl2 <- fl1%>%
  
  ## Combined year, month and day
  mutate(dep_date = make_date(year.x, month,day))%>%
  dplyr::select(-c(year.x, month,day))%>%
  
  ## Remove dep_time, arr_time and arr_delay since they shoudn't be known
  dplyr::select(-c(dep_time, arr_time,arr_delay))%>%
  
  ## Remove name since it is  captured by dest ,
  ## Remove hour, minute and time_hour since they are captured by sched_dep_time
  ## Remove tz, dst, tzone since they are captured by dest
  ## Remove flight and tailnum since there are more than 3000 levels
  dplyr::select(-c(name, hour, minute, time_hour, tz, dst, tzone, flight, tailnum))%>%
  
  # Convert visib, precip as factor variables: table(fl$visib), table(fl$precip)
  # Convert dep_date as numeric
  mutate(
    # visib = as.factor(visib),
    #      precip = as.factor(precip),
    dep_date = as.numeric(dep_date) )



## 3. Analyze Numbric Variable

## Correlation
str(fl2)
fl.cov <- fl2[,-c(4,5,6,14,15,19)]

cor.table <- cor(fl.cov)

cor.table > 0.7 
cor.table[cor.table > 0.7]
# These variables are positively correlated: 
# temp and dewp: 0.883164131 -> remove temp
# distance and air_time: 0.991420774 -> remove distance
# sched_arr_time and sched_dep_time: 0.837658457 -> remove sched_arr_time


cor.table< -0.7
cor.table[cor.table< -0.7]
# These variables are negatively correlated: 
# lon and air_time -> remove lon
# lon and distance
fl3 <- fl2 %>%
  dplyr:: select(-c(temp, distance, sched_arr_time, lon))


## The departure delays variable is highly right-skewed.
plot(density(log(fl2$dep_delay)))
plot(fl2$dep_delay)
plot(density(fl3$dep_delay), main="Kernel Density of Departure Delays")
## scale the dep_delay by ranks 
den <- nrow(fl3)+1
fl3 <- fl3 %>% mutate(dep_delay = rank(dep_delay)/den)
plot(density(fl3$dep_delay))

##
str(fl3)
ggplot(fl3,aes(x=sched_dep_time ,y=dep_delay)) + geom_point(alpha=0.01) + geom_smooth() ## okay
ggplot(fl3,aes(x=log(air_time) ,y=dep_delay)) + geom_point(alpha=0.01) + geom_smooth() ## use log to compress data
ggplot(fl3,aes(x=dewp ,y=dep_delay)) + geom_point(alpha=0.01) + geom_smooth()## okay
ggplot(fl3,aes(x=humid ,y=dep_delay)) + geom_point(alpha=0.01) + geom_smooth()## okay
ggplot(fl3,aes(x=wind_dir ,y=dep_delay)) + geom_point(alpha=0.01) + geom_smooth()## okay
ggplot(fl3,aes(x=wind_speed,y=dep_delay)) + geom_point(alpha=0.01) + geom_smooth()## change in log is not significant
ggplot(fl3,aes(x=lat,y=dep_delay)) + geom_point(alpha=0.01) + geom_smooth()## change in log is not significant
ggplot(fl3,aes(x=log(alt) ,y=dep_delay)) + geom_point(alpha=0.01) + geom_smooth()## use log to compress data
ggplot(fl3,aes(x= dep_date,y=dep_delay)) + geom_point(alpha=0.01) + geom_smooth()## okay
ggplot(fls,aes(x= as.numeric(precip>0),y=dep_delay)) + geom_point(alpha=0.01)+ geom_smooth()## covert to binary variable
ggplot(fl3,aes(x= visib,y=dep_delay)) + geom_point(alpha=0.01)+ geom_smooth() ## okay
fl3 <- fl3%>%
  mutate(air_time = log(air_time),
         alt = log(alt),
         precip = as.numeric(precip>0))

## 4. Categorical Variables:
str(fl3)
ggplot(fl3,aes(x= carrier,y=dep_delay)) + geom_point(alpha=0.01) ## no significant pattern
ggplot(fl3,aes(x= origin,y=dep_delay)) + geom_point(alpha=0.01) ## no significant pattern
ggplot(fl3,aes(x= dest,y=dep_delay)) + geom_point(alpha=0.01) ## no significant pattern
fl4 <- fl3


##### Model #########

n <- nrow(fl4)

#  baseline
var_dd <- var(fl4$dep_delay)

# y <- as.vector(fl4$dep_delay)
# xm <- as.matrix(fl4[, c(-2)])
set.seed(1211)
k <- 5
ii <- (1:n)%%k + 1

ii <- sample(ii)
pr.la <- pr.f <- pr.ri <- pr.st <- pr.gam<- rep(0, n)

# 5 fold
for (j in 1:k) {
  
  ## LASSO and Ridge not applicable since some caregorical variables have more than 100 levels
  # tmp.ri <- cv.glmnet(x = xm[ii != j, ], y = y[ii != j], lambda = lambdas, 
  #                     nfolds = 5, alpha = 0, family = "gaussian")
  # tmp.la <- cv.glmnet(x = xm[ii != j, ], y = y[ii != j], lambda = lambdas, 
  #                     nfolds = 5, alpha = 1, family = "gaussian")
  
  ## Stepwise and Full model have large mspe 
  null <- lm(dep_delay ~ 1, data = fl4[ii != j, ])
  full <- lm(dep_delay ~ ., data = fl4[ii != j, ])
  tmp.st <- stepAIC(null, scope = list(lower = null, upper = full), trace = 0)
  
  
  ## Use mspe since many numeric variables are polynomial 
  form <- formula(dep_delay ~ s(sched_dep_time) + carrier + origin + dest + s(air_time) +
                    s(dewp) + s(humid) + s(wind_dir) + s(wind_speed) + precip + s(visib)+s(lat)+s(alt)+s(dep_date))
  gam_fit <- gam(form, data=fl4[ii != j, ],family=gaussian)
  # pr.ri[ii == j] <- predict(tmp.ri, s = "lambda.min", newx = xm[ii == 
  #                                                                 j, ])
  # pr.la[ii == j] <- predict(tmp.la, s = "lambda.min", newx = xm[ii == 
  #                                                                 j, ])
  pr.st[ii == j] <- predict(tmp.st, newdata = fl4[ii == j, ])
  pr.f[ii == j] <- predict(full, newdata = fl4[ii == j, ])
  pr.gam[ii == j] <- predict(gam_fit, newdata = fl4[ii == j, ])
  
  
}
# mspe.ri <- mean((fl4$dep_delay - pr.ri)^2)
# mspe.la <- mean((fl4$dep_delay - pr.la)^2)
mspe.st <- mean((fl4$dep_delay - pr.st)^2)
mspe.f <- mean((fl4$dep_delay - pr.f)^2)
mspe.gam <- mean((fl4$dep_delay-pr.gam)^2)

# ## Testing and traning 
train_idx <- sample(1:n, ceiling(0.7*n))
training <- fl4[train_idx, ]
test <- fl4[-train_idx, ]

## gam
form <- formula(dep_delay ~ s(sched_dep_time) + carrier + origin + dest + s(air_time) +
                  s(dewp) + s(humid) + s(wind_dir) + s(wind_speed) + precip + s(visib)+s(lat)+s(alt)+s(dep_date))
gam_fit <- gam(form, data=training,family=gaussian)

gam_pred <- predict(gam_fit,newdata=test,n.trees = 1000)
mse_gam <- mean((test$dep_delay-gam_pred)^2)

## Stepwise
null <- lm(dep_delay ~ 1, data = training)
full <- lm(dep_delay ~ ., data = training)
tmp.st <- stepAIC(null, scope = list(lower = null, upper = full), trace = 0)

st_pred <- predict(tmp.st,newdata=test)
mse_st <- mean((test$dep_delay-st_pred)^2)




