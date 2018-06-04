library(readxl)
library(MASS)
library(leaps)
library(DAAG)

install.packages("robustbase")
library(robustbase) ## median function
install.pachages("corrplot")
library(corrplot) ## cor plots (in presentation)

##################################################################################################################################################################
## Preprocessing
##################################################################################################################################################################
setwd("~/Documents/stat analy/project")

csm<- read_excel("2014 and 2015 CSM dataset.xlsx")

num <- csm
#rm(csm)
num <- num[num$Year==2014,]
num <- num[num$Sequel==1,] ## I am setting these constant to make the dataset smaller
dim(num)

num <- num[, setdiff(names(num), c("Movie", "Year", "Sequel", "Genre"))]
dim(num)

## Movie is a list of names
## Year, and Sequel are fixed by me
## Genre is categorical
names(num)[names(num) == 'Aggregate Followers'] <- 'Aggregate_followers'
## easier to call name this way

summary(num) ## Beware of NA's and check distributions
#budget, screens, and aggregate followers have na

## below are two functions that replace the NA values with median
medianWithoutNA<-function(x) {
  median(x[which(!is.na(x))])
}

impute.med <- function(x) {
  z <- median(x, na.rm = TRUE)
  x[is.na(x)] <- z
  return(x)
} ## this function replaces NAs with median values


num <- sapply(num, function(x){
  if(is.numeric(x) & any(is.na(x))){
    impute.med(x)
  } else {
    x
  }
}
) ## this calls the function above
## It was more complicated to replace NAs than I thought.

num <- as.data.frame(num)
num <- as.data.frame(sapply(num, as.numeric))
## the structure of file became char when I ran the impute function
## so I had to change back

corrplot(cor(num))
correl <- as.data.frame(as.table(cor(num)))

a <-which(correl$Freq>0.9 & correl$Freq != 1)
correl[a,] 
## comments and likes are higly correlated
rm(correl)
rm(a)

hist(num$Ratings) ## normal ish
hist(num$Gross)
hist(log(num$Gross)) ## log is better
hist(log(num$Budget)) ## pretty good dist when looking at log scale
hist(num$Screens)
hist(log(num$Screens)) ## either log or no log is ok
hist(num$Sentiment) ## outliers <-10
hist(num$Views)
hist(log(num$Views)) ## log is better
hist(num$Likes)
hist(log(num$Likes)) ## log is better
hist(num$Dislikes)
hist(log(num$Dislikes)) ## log is better
hist(num$Aggregate_followers)
hist(log(num$Aggregate_followers))
## Many above will appear more linear on a log scale. I will confirm this 
## by plotting both 

plot(num)
num[c('Screens','Gross', 'Budget', 'Views', 'Likes', 'Comments', 'Dislikes',
  'Aggregate_followers')][num[c('Screens','Gross', 'Budget', 'Views', 
  'Likes', 'Comments', 'Dislikes','Aggregate_followers')]==0] <- 1
## I am changing all the 0's to 1's for the log scale 

lognum <- num
lognum$Screens <- log(lognum$Screens)
lognum$Gross <- log(lognum$Gross)
lognum$Budget <- log(lognum$Budget)
lognum$Views <- log(lognum$Views)
lognum$Likes <- log(lognum$Likes)
lognum$Comments <- log(lognum$Comments)
lognum$Dislikes <- log(lognum$Dislikes)
lognum$Aggregate_followers <- log(lognum$Aggregate_followers)

names(lognum)[names(lognum) == 'Gross'] <- "log_gross"
names(lognum)[names(lognum) == 'Screens'] <- "log_screens"
names(lognum)[names(lognum) == 'Budget'] <- "log_budget"
names(lognum)[names(lognum) == 'Views'] <- "log_views"
names(lognum)[names(lognum) == 'Likes'] <- "log_likes"
names(lognum)[names(lognum) == 'Dislikes'] <- "log_dislikes"
names(lognum)[names(lognum) == 'Comments'] <- "log_comments"
names(lognum)[names(lognum) == 'Aggregate Followers'] <- "log_agg_fol"

rm(num)
plot(lognum)

##################################################################################################################################################################
## Model Selection
##################################################################################################################################################################

step(lm(log_gross~., data=lognum), direction ="backward") 

fm <- names(lognum[, names(lognum) != "log_gross"]) ## names of
fm <- as.formula(paste("~", paste(fm, collapse = "+"))) ## build
## fm is the formula that contains all the variables 

step(lm(log_gross~., data=lognum), direction ="both", 
     scope = fm)

rm(fm)
best.subset <- regsubsets(log_gross~., data=lognum)
best.subset.summary <- summary(best.subset)
table <-best.subset.summary$outmat
#View(table)
## Note that none of these subsets include log comments with log likes, so 
## Multicollinearity will not ben an issure for these! YAY
## The sixth one is the model suggested by both step functions


plot(best.subset.summary$bic, main="Bic Values for Each Model (4 is Best)")
lines(best.subset.summary$bic)
a <- which(best.subset.summary$bic==min(best.subset.summary$bic))
a ## 4
rm(a)

plot(best.subset.summary$rsq)
lines(best.subset.summary$rsq)
title("Rsq plot for Best Subsets")
## from plot, any in the 5-8 region seem reasonable

a <- which(best.subset.summary$adjr2==max(best.subset.summary$adjr2))
a ## the 6th one
best.subset.summary$adjr2[a] ## 0.5549
plot(best.subset.summary$adjr2)
lines(best.subset.summary$adjr2)
title("Adj Rsq (6th is Max)")
table[a,] ## ratings, budget, screens, views, likes, dislikes

best.subset.summary$cp
#[1] 30.305649 15.580507  8.251434  5.212105  3.489109  4.112494  6.040459  8.001954
ideal <- 2:9
dif <- as.vector(best.subset.summary$cp)-ideal
plot(dif)
lines(dif)
abline(0,0)
title("Differences Cp values vs. P")
## cp recommends that I consider model 4

## I am callling all the models in regsubsets. Note that I am using some notation
## that collapses the variables from the original table
##(easier than typing all the names)
p2 <- lm(as.formula(paste("log_gross ~", paste(names(table[1,])[table[1,]=="*"], collapse = "+"))), data=lognum)
p3 <- lm(as.formula(paste("log_gross ~", paste(names(table[2,])[table[2,]=="*"], collapse = "+"))), data=lognum)
summary(p3)
p4 <- lm(as.formula(paste("log_gross ~", paste(names(table[3,])[table[3,]=="*"], collapse = "+"))), data=lognum)
summary(p4)
p5 <- lm(as.formula(paste("log_gross ~", paste(names(table[4,])[table[4,]=="*"], collapse = "+"))), data=lognum)
summary(p5)
p6 <- lm(as.formula(paste("log_gross ~", paste(names(table[5,])[table[5,]=="*"], collapse = "+"))), data=lognum)
summary(p6)
p7 <- lm(as.formula(paste("log_gross ~", paste(names(table[6,])[table[6,]=="*"], collapse = "+"))), data=lognum)
summary(p7) ## not all significant
p8 <- lm(as.formula(paste("log_gross ~", paste(names(table[7,])[table[7,]=="*"], collapse = "+"))), data=lognum)
summary(p8) ## not all significant
p9 <- lm(as.formula(paste("log_gross ~", paste(names(table[8,])[table[8,]=="*"], collapse = "+"))), data=lognum)
summary(p9) ## not all significant

## I am making a data frame that stores press and aic values 
val <- as.data.frame(cbind(rep(NA, 8), rep(NA, 8)))
val[1,1] <-press(p2)
val[2,1] <-press(p3)
val[3,1] <-press(p4)
val[4,1] <-press(p5)
val[5,1] <-press(p6)
val[6,1] <-press(p7)
val[7,1] <-press(p8)
val[8,1] <-press(p9)

val[1,2] <-AIC(p2)
val[2,2] <-AIC(p3)
val[3,2] <-AIC(p4)
val[4,2] <-AIC(p5)
val[5,2] <-AIC(p6)
val[6,2] <-AIC(p7)
val[7,2] <-AIC(p8)
val[8,2] <-AIC(p9)
colnames(val) <- c("press", "Aic")
a <- which(val$press==min(val$press))
a ## the 5th
plot(val$press)
lines(val$press)
title("Press outputs from Regsubsets Models")
val$press[a]

a <- which(val$Aic==min(val$Aic))
a ## the fifth one
val$Aic[a]

plot(val$Aic)
lines(val$Aic)
title("AIC Values for Each Model (5th is Best)")

##################################################################################################################################################################
## Modeling
##################################################################################################################################################################

anova(p5) ## mse is 3.91
## Ratings, Budget, Screens, Likes, Views show some significance 


summary(p5) ## P value for the f statistic is 2.2 e -16 
## Ratings, Budget, Screens, Views 
## (we can conduct hypothesis test)
## Adjusted R squared 0.55 ish (We'll find out that we can get this with simpler models)
qqnorm(p5$residuals, main = "QQ Plot of Residuals") ## heavy tailing of residual
qqline(p5$residuals)

resid <- exp(lognum$log_gross)-exp(p5$fitted.values)
plot(exp(p5$fitted.values), resid)
abline(0,0)
title("Residuals vs. Fitted Transform Back")

plot(p5$fitted.values, p5$residuals, main="Residuals vs. Fitted")
abline(0,0) ## not independent, no equal variances

## I want to know the prediction intervals, to see how well I can predict 
## the gross profits of a movie. 
pred <- predict(p5, interval="prediction")
pred <- as.data.frame(pred) 
ints <- exp(pred$upr)-exp(pred$lwr)
plot(ints)
boxplot(ints, outline = FALSE) ## prediction intervals are huge

comp <-c(0.5*median(ints), -0.5*median(ints)) ## I want to compare the median 
## prediction interval to the distribution of Gross Profits

boxplot(comp, exp(lognum$log_gross)-median(exp(lognum$log_gross)), 
        outline = TRUE, names = c("Median Pred Ints", "Gross Budget"))

## prediction intervals are larger than the distribution of the output variable

fivenum(exp(lognum$log_gross))
plot(exp(pred$upr)-exp(pred$lwr), main=("Width of Prediction Intervals"))

##################################################################################################################################################################
## Cross Validation
##################################################################################################################################################################


a <- 4 ## index in the table I've chosen
set.seed(3007998)
lognum$k5 <- as.integer(runif(nrow(lognum))*5)
summary(as.factor(lognum$k5))

yhat <- rep(NA, nrow(lognum))
for(i in 0:4){
  fit <- lm(as.formula(paste("log_gross ~", paste(names(table[a,])[table[a,]=="*"], 
      collapse = "+"))), data=lognum, subset=(k5!=i))
  yhat[lognum$k5==i] <- predict(fit, lognum[lognum$k5==i,])
}

train_mse1 <- sum(fit$residuals^2)/length(fit$residuals)
train_mse1 ## 3.65244
### mean square error of train set

mspe1 <- sum((lognum$log_gross-yhat)^2)/length(yhat)
mspe1 ## 4.005398

## MSE
sum((exp(fit$fitted.values)-exp(lognum[lognum$k5 !=i,]$log_gross))^2)/length(fit$residuals)
## 4 e 15 

## MSPE
sum((exp(lognum$log_gross)-exp(yhat))^2)/length(yhat)
## 4.77 e 15

### MSE is approximately MSPE


error <-exp(lognum$log_gross)-exp(yhat)
errorpl <- error ## for boxplot comp
centeredgross <-exp(lognum$log_gross)-median(exp(lognum$log_gross))

boxplot(cbind(centeredgross, errorpl), outline=FALSE, names=c("Gross (Centered)", "Pred Error"))
title("Compare Distributions")
