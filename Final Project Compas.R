rm(list = ls())
#Final Project Math 23C
#Lavanya S. and Shuvom S.
####POINT 22: team consists of exactly 2 members

#setwd("/Users/shuvomsadhuka/Documents/GitHub/compas_analysis")
compas <- read.csv ("compass/compas-scores-raw.csv"); #View(compas)
library(ggplot2)
library(gmodels)
library(stats4)

compas[,DisplayText == 'Risk of Violence']

#messy but there are two clear clusters we can isolate
ggplot(compas, aes(x=RawScore)) + geom_histogram()
####POINT 11: ggplot
#let's use R's built-in k-means cluster function
clusters <- kmeans(compas$RawScore, 2); clusters
#storing the cluster of each point in the dataframe
compas$cluster <- clusters$cluster


#first cluster looks pretty bell shaped
scores <- compas[which(compas$cluster == 1),]; head(scores)
max <- max(scores$RawScore)
min <- min(scores$RawScore)
#squeezing the values to be between 0 and 1
scores$score <- (max - scores$RawScore) / (max - min)
plot <- ggplot(scores, aes(score)) + geom_histogram(aes(y = stat(density))); plot
mu <- mean(scores$score); mu
var <- var(scores$score); var
#used Wolfram Alpha to find parameters for a beta distribution by solving a system of equations
param1 <- 15162/3725
param2 <- 11913/3725
#let's try dbeta
stat1 <- stat_function(fun = dbeta, args = list(param1, param2), lwd = 1, col = "red")
stat2 <- stat_function(fun = dnorm, args = list(mu, sqrt(var)), lwd = 1, col = "red")
#that looks quite accurate!
plot + stat1
plot + stat2
####REQUIRED: probability density curve overlaid on a histogram

#second cluster 
scores2 <- compas[which(compas$cluster == 2),]
#translating the data to the origin
#and transforming it to lie between 0 and 10
scores2$score <- (scores2$RawScore - min(scores2$RawScore)) / 4
mu <- mean(scores2$score) ; mu
var <- var(scores2$score); var
plot <- ggplot(scores2, aes(score)) + geom_histogram(aes(y = stat(density)), binwidth = 0.25); plot
#let's try dgamma
stat <- stat_function(fun = dgamma, args = list(mu), lwd = 1, col = "red")
#dgamma appears to fit!
plot + stat

#let's filter our data by race
scores_black <- compas[which(compas$Ethnic_Code_Text == 'African-American'),]; head(scores_black)

#here's some preliminary summary statistics for blacks, whites
mean(scores_black$RawScore)
scores_white <- compas[which(compas$Ethnic_Code_Text == 'Caucasian'),]; head(scores_white)
mean(scores_white$RawScore)
diff <- mean(scores_black$RawScore) - mean(scores_white$RawScore); diff

#for this project, we'll group people of color as "nonwhite" and compare to white
scores_arabic <- compas[which(compas$Ethnic_Code_Text == 'Arabic'),]
scores_asian <- compas[which(compas$Ethnic_Code_Text == 'Asian'),]
scores_hisp <- compas[which(compas$Ethnic_Code_Text == 'Hispanic'),]
scores_other <- compas[which(compas$Ethnic_Code_Text == 'Other'),]

#this is all the nonwhite people in the dataset
scores_nonwhite <- compas[which(!compas$Ethnic_Code_Text == 'Caucasian'),]
#let's look at what the data looks like
head(scores_nonwhite)

#let's do logistic regression on raw score and race (Bernoulli variable) - can we use 
#the risk of violence raw score to predict race? - if so, we've shown the algo might be biased
####POINT 15: logistic regression
violence <- compas[which(compas$DisplayText == 'Risk of Violence'),]
violence$race <- ifelse(compas$Ethnic_Code_Text == "Caucasian", 0, 1)
score <- violence$RawScore
MLL<- function(alpha, beta) {
  -sum( log( exp(alpha+beta*score)/(1+exp(alpha+beta*score)) )*violence$race
        + log(1/(1+exp(alpha+beta*score)))*(1-violence$race) )
}
plot(score,violence$race)  #not a great candidate for a straight-line approximation, but let's try
b <- cov(score,violence$race)/var(score)    #easy way to get the slope
#Here is the formula for the intercept
a <- mean(violence$race) - b*mean(score);a  
#We can add this regression line to the plot of the data
abline(a, b, col = "red")
#This is bad --- it gives a
results<-mle(MLL, start = list(alpha = 0, beta = 0)) #an initial guess is required
results@coef
curve( exp(results@coef[1]+results@coef[2]*x)/ (1+exp(results@coef[1]+results@coef[2]*x)),col = "blue", add=TRUE)

#let's also try a linear regression
####POINT 14: linear regression
model <- lm(compas$RawScore ~ compas$Person_ID)
summary(model)
ggplot(compas, aes(x=Person_ID, y=RawScore)) + 
  geom_point()+
  geom_smooth(method = lm)

#the problem is that we're pooling the data by types of risk -- we need to look individually at 
#each risk type
no_appear <- compas[which(compas$DisplayText == 'Risk of Failure to Appear'),]
violence <- compas[which(compas$DisplayText == 'Risk of Violence'),]
recid <- compas[which(compas$DisplayText == 'Risk of Recidivism'),]
model <- lm(no_appear$RawScore ~ no_appear$Person_ID)
summary(model)
ggplot(no_appear, aes(x=Person_ID, y=RawScore)) + 
  geom_point()+
  geom_smooth(method = lm)

model <- lm(violence$RawScore ~ violence$Person_ID)
summary(model)
ggplot(violence, aes(x=Person_ID, y=RawScore)) + 
  geom_point()+
  geom_smooth(method = lm)

model <- lm(recid$RawScore ~ recid$Person_ID)
summary(model)
ggplot(recid, aes(x=Person_ID, y=RawScore)) + 
  geom_point()+
  geom_smooth(method = lm)


#we can repeat our analysis by subdividing our population into each individual type of risk
type_of_risk <- function(x, y){
  library(ggplot2)
  stopifnot(y < 2)
  
  if (y == 0) #y == 0 represents where we have subdivided the population
  {
    type <- compas[which(compas$DisplayText == x),]; #View(type)
    type_white <- type[which(type$Ethnic_Code_Text == 'Caucasian'),]
    type_nonwhite <- type[which(!type$Ethnic_Code_Text == 'Caucasian'),]
  }
  
  if (y == 1) #y == 1 represents the pooled dataset
  {
    type <- compas
    type_white <- type[which(type$Ethnic_Code_Text == 'Caucasian'),]
    type_nonwhite <- type[which(!type$Ethnic_Code_Text == 'Caucasian'),]
  }
 
  
  #get the lengths of the non-white and white data
  nw = length(type_white$RawScore); nw
  nn = length(type_nonwhite$RawScore); nn
  total = nw+nn
  length(type$RawScore)
  
  #here's our first statistical test: a permutation test!
  ####REQUIRED: permutation test
  #we want to do a 2-sample permutation test
  #first we compute the actual difference in means
  actual = mean(type_nonwhite$RawScore) - mean(type_white$RawScore)
  diffs <- vector()
  
  for (i in 1:5000){
    all_scores <- c(type_white$RawScore, type_nonwhite$RawScore); length(all_scores)
    #sample nw incidies for the white mean
    sampled_indicies <- sample(1:total, nw, replace = FALSE); sampled_indicies
    
    sampled_white <- all_scores[sampled_indicies]
    mean(sampled_white)
    
    #the rest of the individuals will be in the nonwhite sample
    sampled_nonwhite<- all_scores[-sampled_indicies];
    mean(sampled_nonwhite)
    diff <- mean(sampled_white) - mean(sampled_nonwhite)
    
    diffs <- c(diffs, diff)
  }
  
  diffs_df <- data.frame(x = diffs)
  
  ####POINT 11 - we can also make the plot look nicer using ggplot
  print(ggplot(diffs_df, aes(x=x)) + 
    geom_histogram(binwidth=0.01, colour = "black") + geom_vline(xintercept = actual, colour = "red")
    + labs(title = paste("Permutation Test for ", x)))
  
  #to calculate the p-value, we use the empirical cdf
  percentile <- ecdf(diffs)
  print((1 - percentile(0.2689452))*2)
  
  type_white$id <- 'white'
  type_nonwhite$id <- 'non-white'
  
  #here's an example of a variation on a graph that we've seen before but haven't done in class 
  ####POINT 19 - a graphical display different from the ones in class
  Lengths <- data.frame(rbind(type_nonwhite, type_white))
  print(ggplot(Lengths, aes(RawScore, fill = id)) + 
    geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
    + labs(title = paste("Raw Scores for ", x)))
  
  
  ##t-test
  print(t.test(type_white$RawScore, type_nonwhite$RawScore))
  
  #lets recreate the t-test manually and then check against the R package (above)
  nonwhite_mean <- mean(type_nonwhite$RawScore)
  white_mean <- mean(type_white$RawScore)
  nonwhite_sd <- sqrt(var(type_nonwhite$RawScore))
  white_sd <- sqrt(var(type_white$RawScore))
  
  #this is the standard deviation for a two-sample t-test with unequal populations and variances
  std <- sqrt(((nw - 1)*white_sd^2 + (nn - 1)*nonwhite_sd^2)/(nw + nn - 2))*(sqrt(1/nw + 1/nn)); std
  
  #this is the t-stat
  t_stat <- (nonwhite_mean - white_mean)/std; print(t_stat)
  
  #the degrees of freedom
  deg.freedom = nw + nn - 2
  print(pt(t_stat, deg.freedom, lower.tail = FALSE, log.p = FALSE)) #the p-value is sufficiently low for us to
  #reject the null hypothesis that no racial disparity
  
  #we can also try a z-test, using the theory
  diff_in_means = nonwhite_mean - white_mean; diff_in_means
  std_norm = sqrt(nonwhite_sd^2/nn + white_sd^2/nw)
  z_stat = diff_in_means/std_norm; z_stat
  print(pnorm(z_stat, 0, 1, lower.tail = FALSE))
  
  #let's compute a confidence interval 
  ####POINT 20: confidence interval
  theta = diff_in_means
  sigma_sq = var(all_scores); sigma_sq
  conf_lower = theta - 1.96 * (sqrt(sigma_sq))/(sqrt(total)); print(conf_lower) #bounds of conf interval
  conf_upper = theta + 1.96 * (sqrt(sigma_sq))/(sqrt(total)); print(conf_upper) #based on normal approximation
  
  
  t.est <- t.test(type_nonwhite$RawScore, type_white$RawScore, var.equal=FALSE)$stat
  print(t.est)
  #now let's make a bootstrapped t-test
  means_nonwhite <- vector()
  means_white <- vector()
  
  #the bootstrap can also cross-check against sensitivity to outliers since
  #outliers have a small chance of being included in any given bootstrapped dataset
  #this counts as another simulation method
  b <- function(){
    A <- sample(type_nonwhite$RawScore, nn, replace=T)  
    B <- sample(type_white$RawScore, nw, replace=T) 
    stud_test <- t.test(A, B, var.equal=FALSE)
    stud_test
    
    return(stud_test$stat)
  }
  t.stat.vect = vector(length=10000)
  t.vect <- replicate(10000, b())
  print(1 - mean(t.est>t.vect))
}
type_of_risk('Risk of Violence', 0)
type_of_risk('Risk of Recidivism', 0)
type_of_risk('Risk of Failure to Appear', 0)
type_of_risk('Pooled', 1)

violence <- compas[which(compas$DisplayText == 'Risk of Violence'),]; View(violence)
violence_white <- compas[which(compas$Ethnic_Code_Text == 'Caucasian'),]

#are race and a display text of "Risk of Violence" independent?
#extracting logical columns
risk_Log <- compas$DisplayText == "Risk of Violence"; sum(risk_Log)
race_Log <- compas$Ethnic_Code_Text != "Caucasian"; sum(race_Log)
#REQUIRED: two categorical columns
dataLog <- data.frame(risk_Log, race_Log)
#a contingency table showing all 4 options
Obs <- table (dataLog$risk_Log, dataLog$race_Log); Obs
#what we would expect if the factors are independent
Expected <- outer(rowSums(Obs), colSums(Obs))/sum(Obs); Expected
#REQUIRED: analysis of contingency tables
#WOAH these are the exact same 
#chi-sq: p value is 1 meaning that there is 100% chance that race and risk of violence
#as display text are independent
chisq.test(dataLog$risk_Log, dataLog$race_Log)
#Paul's method of calculating chi-sq value
ChiSq <-function(Obs,Exp){
  sum((Obs-Exp)^2/Exp)
}
#same chi-sq statistic and p value as above
CSq <- ChiSq(Obs, Expected); CSq   
pchisq(CSq, df = 3, lower.tail = FALSE)
#REQUIRED: p value using a distribution function
#POINT 9: relationship that we expected to be statistically significant but was not

#are race and a display text of "Risk of Recidivism" independent?
#extracting logical columns
recid_Log <- compas$DisplayText == "Risk of Recidivism"; sum(recid_Log)
dataLog$recid_Log <- recid_Log
#a contingency table showing all 4 options
Obs <- table (dataLog$recid_Log, dataLog$race_Log); Obs
#what we would expect if the factors are independent
Expected <- outer(rowSums(Obs), colSums(Obs))/sum(Obs); Expected
#WOAH these are the exact same 
#chi-sq: p value is 1 meaning that there is 100% chance that race and risk of violence
#as display text are independent
chisq.test(dataLog$recid_Log, dataLog$race_Log)
#Paul's method of calculating chi-sq value
ChiSq <-function(Obs,Exp){
  sum((Obs-Exp)^2/Exp)
}
#same chi-sq statistic and p value as above
CSq <- ChiSq(Obs, Expected); CSq   
pchisq(CSq, df = 3, lower.tail = FALSE)

#are race and a display text of "Risk of Failure to Appear" independent?
#extracting logical columns
appear_Log <- compas$DisplayText == "Risk of Recidivism"; sum(appear_Log)
dataLog$appear_Log <- appear_Log
#a contingency table showing all 4 options
Obs <- table (dataLog$appear_Log, dataLog$race_Log); Obs
#what we would expect if the factors are independent
Expected <- outer(rowSums(Obs), colSums(Obs))/sum(Obs); Expected
#WOAH these are the exact same 
#chi-sq: p value is 1 meaning that there is 100% chance that race and risk of violence
#as display text are independent
chisq.test(dataLog$appear_Log, dataLog$race_Log)
#Paul's method of calculating chi-sq value
ChiSq <-function(Obs,Exp){
  sum((Obs-Exp)^2/Exp)
}
#same chi-sq statistic and p value as above
CSq <- ChiSq(Obs, Expected); CSq   
pchisq(CSq, df = 3, lower.tail = FALSE)

#our three contingency tables displayed nicely
#Risk of Violence versus Race (Caucasian or not)
CrossTable(dataLog$risk_Log, dataLog$race_Log, dnn= c("Caucasian", "Risk of Violence"), prop.t=FALSE, prop.r=FALSE, prop.c=FALSE, prop.chisq = FALSE)
#Risk of Recidivism versus Race (Caucasian or not)
CrossTable(dataLog$recid_Log, dataLog$race_Log, dnn= c("Caucasian", "Risk of Recidivism"), prop.t=FALSE, prop.r=FALSE, prop.c=FALSE, prop.chisq = FALSE)
#Risk of Failure to Appear versus Race (Caucasian or not)
CrossTable(dataLog$appear_Log, dataLog$race_Log, dnn= c("Caucasian", "Risk of Failure to Appear"), prop.t=FALSE, prop.r=FALSE, prop.c=FALSE, prop.chisq = FALSE)
#REQUIRED: graphical contingency tables

#another way of showing race and display text are independent - 
#covariance and correlation are close to 0
#race and risk of violence
cov(dataLog$race_Log, dataLog$risk_Log)
cor(dataLog$race_Log, dataLog$risk_Log)
#race and risk of recidivism
cov(dataLog$race_Log, dataLog$recid_Log)
cor(dataLog$race_Log, dataLog$recid_Log)
#race and risk of failure to appear
cov(dataLog$race_Log, dataLog$appear_Log)
cor(dataLog$race_Log, dataLog$appear_Log)
#all are close to 0
#POINT 16: Covariance and correlation

#barplot of various display texts
#they are all perfectly equal! weird!
plot <- ggplot(data = compas, aes(x = factor(DisplayText))) + 
  geom_bar(stat="count", width=0.7, fill="steelblue") + theme_minimal() +
  ggtitle("Display Text vs Frequency") + xlab("Display Text") + ylab("Frequency"); plot
#REQUIRED: barplot

#probability of a white person getting a display text of "Risk of Failure to Appear" 
p1<- length(which(compas$Ethnic_Code_Text == "Caucasian" & compas$DisplayText == "Risk of Failure to Appear"))/length(compas$Ethnic_Code_Text)
p1
#probability of a non-white person getting a display text of "Risk of Failure to Appear" 
p2 <- length(which(compas$Ethnic_Code_Text != "Caucasian" & compas$DisplayText == "Risk of Failure to Appear"))/length(compas$Ethnic_Code_Text)
p2
#probability of a white person getting a display text of "Risk of Violence" 
p3 <- length(which(compas$Ethnic_Code_Text == "Caucasian" & compas$DisplayText == "Risk of Violence"))/length(compas$Ethnic_Code_Text)
p3
#probability of a non-white person getting a display text of "Risk of Violence" 
p4 <- length(which(compas$Ethnic_Code_Text != "Caucasian" & compas$DisplayText == "Risk of Violence"))/length(compas$Ethnic_Code_Text)
p4
#probability of a white person getting a display text of "Risk of Recidivism" 
p5 <- length(which(compas$Ethnic_Code_Text == "Caucasian" & compas$DisplayText == "Risk of Recidivism"))/length(compas$Ethnic_Code_Text)
p5
#probability of a non-white person getting a display text of "Risk of Recidivism" 
p6 <- length(which(compas$Ethnic_Code_Text != "Caucasian" & compas$DisplayText == "Risk of Recidivism"))/length(compas$Ethnic_Code_Text)
p6
#in each case, probability of a person of color getting the corresponding display text is higher
#should equal 1
p1+p2+p3+p4+p5+p6
#it does 

