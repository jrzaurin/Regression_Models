data(mtcars)
mtcars$am <-  as.factor(mtcars$am)
mtcars$vs <-  as.factor(mtcars$vs)
num.cols <- c(1,3,4,5,6,7)
mtcars.num <- mtcars[,num.cols]

library(car)
library(MASS)

#simple panel plot
pairs(mtcars.num, main = "Scatter plot for numeric variables")

#a more sophisticated design
#code adapted from http://www.statmethods.net/graphs/scatterplot.html
library(gclus)
library(cluster)
mt.cor <- abs(cor(mtcars.num)) #get the correlation matrix
mt.col <- dmat.color(mt.cor) # asign colors
# reorder variables so those with highest correlation
# are closest to the diagonal
mt.s <- order.single(mt.cor) 
cpairs(mtcars.num, mt.s, panel.colors=mt.col, gap=.5,
main="Variables Ordered and Colored by Correlation" )

#t.test for the am and the vs variables
am0 <- mtcars$mpg[mtcars$am == 0]
am1 <- mtcars$mpg[mtcars$am == 1]
vs0 <- mtcars$mpg[mtcars$vs == 0]
vs1 <- mtcars$mpg[mtcars$vs == 1]
t.test(am0, am1, paired = FALSE, alternative="two.sided", var.equal=FALSE)
t.test(vs0, vs1, paired = FALSE, alternative="two.sided", var.equal=FALSE)

#anova for the rest of the categorical variables with more that two groups. 
aov.cyl <- aov(mpg ~ as.factor(cyl), data = mtcars); summary(aov.cyl)
aov.gear <- aov(mpg ~ as.factor(gear), data = mtcars); summary(aov.gear)
aov.carb <- aov(mpg ~ as.factor(carb), data = mtcars); summary(aov.carb)

#a more complete version of the ANOVA output is provided by
#this customized inference function downloaded from:
#http://bit.ly/dasi_inference
#source("http://bit.ly/dasi_inference")
#there are some caveats regarding the variance and the skewness of some
#of the variables. ANOVA is usually robust against skewness
source("statistics-lab_resources-inference.R") #if already downloaded

inference(mtcars$mpg, as.factor(mtcars$cyl), est = "mean", type = "ht", alternative = "greater", method = "theoretical")
inference(mtcars$mpg, as.factor(mtcars$gear), est = "mean", type = "ht", alternative = "greater", method = "theoretical")
mt.carb <- mtcars[mtcars$carb != 6 & mtcars$carb != 8,]
inference(mt.carb$mpg, as.factor(mt.carb$carb), est = "mean", type = "ht", alternative = "greater", method = "theoretical")

#MODEL SELECTION
full.model <- lm(mpg~., data = mtcars)
#performing a stepwise selection by both
#forward and backward exact AIC (Akaike Information Criterion)
#http://en.wikipedia.org/wiki/Akaike_information_criterion
step <- stepAIC(full.model, direction="both", trace = FALSE); summary(step)

#check the variance inflation. Nit entirely consistent with step. Variability inflation is not always
#the best selection criterion.
mtcars.vif <- sqrt(vif(full.model))

#Model selection by exhaustive search, forward or backward stepwise, or sequential replacement. Idea from http://www.statmethods.net/stats/regression.html
#Some metrics will not work with factors. Load data again as numeric.
#still, these will provide valuable information
rm(list=ls())
data(mtcars)
library(leaps)
leaps<-regsubsets(mpg ~ . , data=mtcars, nbest=1)
# view results 
summary(leaps)
# plot statistic rsq and adjr2
library(cluster)
library(car) #already called. Just in case you start in this section
plot1 <- subsets(leaps, statistic= "rsq", legend = FALSE) #If does not work. Close and open R or equivalently
plot2 <-subsets(leaps, statistic = "adjr2", legend = FALSE) #clean all data store before you start regsubsets
#if legend = TRUE (default) then the legend in placed in the plot with the mouse

#assessing relative importance in linear models.
rm(list=ls())
data(mtcars)
full.model <- lm(mpg~., data = mtcars)
library(relaimpo)
relimp <- calc.relimp(full.model,type=c("last","pratt"),rela=TRUE)
#bootstrap replicates. Note: larger bootstrap replicates will incurr in
#a singularity
boot <- boot.relimp(full.model, b = 100, type = c("last", "pratt"), rank = TRUE, diff = TRUE, rela = TRUE)
plot(booteval.relimp(boot,sort=TRUE))

#Model selection considering factor variables just am and vs
rm(list=ls())
data(mtcars)
mtcars$am <-  as.factor(mtcars$am)
mtcars$vs <-  as.factor(mtcars$vs)
fit1 <- lm(mpg ~ wt + qsec + am, data = mtcars)

#checking the statistics
layout(matrix(c(1,2,3,4),2,2))
plot(fit1)

#95% confidence intervals
coeff <- summary(fit1)$coeff
Intercept.ci <- coeff[1,1] + (c(-1,1) * qt(0.975, df = fit1$df) * coeff[1,2])
wt.ci <-  coeff[2,1] + (c(-1,1) * qt(0.975, df = fit1$df) * coeff[2,2])
qsec.ci <-  coeff[3,1] + (c(-1,1) * qt(0.975, df = fit1$df) * coeff[3,2])
am.ci <-  coeff[4,1] + (c(-1,1) * qt(0.975, df = fit1$df) * coeff[4,2])

#the following code will allows to play a little with a 3D plot.
#points are labelled based on transmission. Red = manual, black = automatic 
library(rgl)
plot3d(mtcars$wt, mtcars$qsec, mtcars$mpg, col=(mtcars$am + 1), size=10, xlab = "weight (lb/1000)", ylab = "1/4 of a mile time (sec)", zlab = "miles per gallon") #I personally prefer the option box = FALSE

#The results change if all the numeric+discrete variables are considered factors

data(mtcars)
cols <- c(2,8,9,10,11)
mtcars[,cols] <- data.frame(apply(mtcars[cols], 2, as.factor))
full.model <- lm(mpg~., data = mtcars)
step <- stepAIC(full.model, direction="both", trace = FALSE); summary(step)

fit2 <- lm(mpg ~ cyl + hp + wt + am, data = mtcars)

layout(matrix(c(1,2,3,4),2,2))
plot(fit2)

coeff <- summary(fit2)$coeff
Intercept.ci <- coeff[1,1] + (c(-1,1) * qt(0.975, df = fit2$df) * coeff[1,2])
cyl6.ci <-  coeff[2,1] + (c(-1,1) * qt(0.975, df = fit2$df) * coeff[2,2])
cyl8.ci <-  coeff[3,1] + (c(-1,1) * qt(0.975, df = fit2$df) * coeff[3,2])
hp.ci <- coeff[4,1] + (c(-1,1) * qt(0.975, df = fit2$df) * coeff[4,2])
wt.ci <-  coeff[5,1] + (c(-1,1) * qt(0.975, df = fit1$df) * coeff[5,2])
am.ci <-  coeff[6,1] + (c(-1,1) * qt(0.975, df = fit2$df) * coeff[6,2])
