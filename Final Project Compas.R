#Final Project
#Lavanya S. and Shuvom S.
#POINT 22: team consists of exactly 2 members
library(ggplot2)
library(gmodels)

compass <- read.csv ("compass/compas-scores-raw.csv"); head(compass)
#REQUIRED: dataframe
compas <- data.frame(compass); head(compas)

#messy but there are two clear clusters we can isolate
ggplot(compas, aes(x=RawScore)) + geom_histogram()
#REQUIRED: histogram

#first cluster -> scores < 7 (this is an estimate, 
#should we try using a clustering algorithm like k-means or something?)
scores <- compas[which(compas$RawScore < 7),]; head(scores)
#REQUIRED: numerical column
max <- max(scores$RawScore)
min <- min(scores$RawScore)
#squeezing the values to be between 0 and 1
scores$score <- (max - scores$RawScore) / (max - min)
#histogram using ggplot
plot <- ggplot(scores, aes(score)) + geom_histogram(aes(y = stat(density))); plot
mu <- mean(scores$score); mu
var <- var(scores$score); var
#used Wolfram Alpha to find parameters for a beta distribution by solving a system of equations
param1 <- 15162/3725
param2 <- 11913/3725
#let's try dbeta
stat <- stat_function(fun = dbeta, args = list(param1, param2), lwd = 1, col = "red")
#that looks quite accurate!
plot + stat
#REQUIRED: probabiity density function overlaid on a histogram

#second cluster -> scores > 7
scores2 <- compas[which(compas$RawScore >= 7),]
#translating the data to the origin
#and transforming it to lie between 0 and 10
scores2$score <- (scores2$RawScore - min(scores2$RawScore)) / 4
mu <- mean(scores2$score) ; mu
var <- var(scores2$score); var
plot <- ggplot(scores2, aes(score)) + geom_histogram(aes(y = stat(density)), binwidth = 0.25); plot
#let's try dgamma
stat <- stat_function(fun = dgamma, args = list(mu), lwd = 1, col = "red")
#POINT 6: probability distribution function other than binomial, normal, or chi-square
#POINT 11: graphics using ggplot
#dgamma appears to fit!
plot + stat

#View(scores)
scores_black <- compas[which(compas$Ethnic_Code_Text == 'African-American'),]
#View(scores_black)
mean(scores_black$RawScore)
scores_white <- compas[which(compas$Ethnic_Code_Text == 'Caucasian'),]
mean(scores_white$RawScore)
diff <- mean(scores_black$RawScore) - mean(scores_white$RawScore); diff

n0 = length(scores_white$RawScore)
n1 = length(scores_black$RawScore)

all_scores <- c(scores_white$RawScore, scores_black$RawScore)
diffs <- vector()
for (i in 1:5000){
  sampled_white <- sample(all_scores, n0, replace = FALSE)
  sampled_black <- all_scores[! all_scores %in% sampled_white]
  diffs <- c(diffs, mean(sampled_black) - mean(sampled_white))
}

hist(diffs, color = 'red')
abline(v = diff)
percentile <- ecdf(diffs)
(1 - percentile(0.5879))*2

hist(scores_white$RawScore)
hist(scores_black$RawScore)
scores_white$id <- 'white'
scores_black$id <- 'black'
#df <- data.frame(white = scores_white$RawScore, black = scores_black$RawScore)
Lengths <- data.frame(rbind(scores_black, scores_white))

ggplot(Lengths, aes(RawScore, fill = id)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')

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

#View(scores)
scores_black <- compas[which(compas$Ethnic_Code_Text == 'African-American' ||
                               compas$Ethnic_Code_Text == 'African-Am'),]
#View(scores_black)
mean(scores_black$RawScore)
scores_white <- compas[which(compas$Ethnic_Code_Text == 'Caucasian'),]
mean(scores_white$RawScore)
diff <- mean(scores_black$RawScore) - mean(scores_white$RawScore); diff
scores_arabic <- compas[which(compas$Ethnic_Code_Text == 'Arabic'),]
scores_asian <- compas[which(compas$Ethnic_Code_Text == 'Asian'),]
scores_hisp <- compas[which(compas$Ethnic_Code_Text == 'Hispanic'),]
scores_other <- compas[which(compas$Ethnic_Code_Text == 'Other')]

scores_nonwhite <- compas[which(!compas$Ethnic_Code_Text == 'Caucasian'),]
head(scores_nonwhite)

nw = length(scores_white$RawScore); nw
nn = length(scores_nonwhite$RawScore); nn
total = nw+nn
length(compas$RawScore)
actual = mean(scores_nonwhite$RawScore) - mean(scores_white$RawScore)
diffs <- vector()

for (i in 1:5000){
  all_scores <- c(scores_white$RawScore, scores_nonwhite$RawScore); length(all_scores)
  sampled_indicies <- sample(1:total, nw, replace = FALSE); sampled_indicies
  
  sampled_white <- all_scores[sampled_indicies]
  mean(sampled_white)
  
  sampled_nonwhite<- all_scores[-sampled_indicies]; #print(sampled_nonwhite)
  mean(sampled_nonwhite)
  diff <- mean(sampled_white) - mean(sampled_nonwhite)
  #print(diff)
  
  diffs <- c(diffs, diff)
}
diffs
hist(diffs, color = 'red')
abline(v = actual)
actual
diffs_df <- data.frame(x = diffs)
ggplot(diffs_df, aes(x=x)) + 
  geom_histogram(binwidth=0.01, colour = "black") + geom_vline(xintercept = 0.2689452, colour = "red")

percentile <- ecdf(diffs)
(1 - percentile(0.2689452))*2

hist(scores_white$RawScore)
hist(scores_nonwhite$RawScore)
scores_white$id <- 'white'
scores_nonwhite$id <- 'non-white'
#df <- data.frame(white = scores_white$RawScore, black = scores_black$RawScore)
Lengths <- data.frame(rbind(scores_nonwhite, scores_white))

ggplot(Lengths, aes(RawScore, fill = id)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')

##t-test
t.test(scores_white$RawScore, scores_nonwhite$RawScore)
#lets recreate the t-test manually
nonwhite_mean <- mean(scores_nonwhite$RawScore)
white_mean <- mean(scores_white$RawScore)
nonwhite_sd <- sqrt(var(scores_nonwhite$RawScore))
white_sd <- sqrt(var(scores_white$RawScore))

std <- sqrt(((nw - 1)*white_sd^2 + (nn - 1)*nonwhite_sd^2)/(nw + nn - 2))*(sqrt(1/nw + 1/nn)); std

t_stat <- (nonwhite_mean - white_mean)/std; t_stat

deg.freedom = nw + nn - 2
pt(t_stat, deg.freedom, lower.tail = TRUE, log.p = FALSE)*2

#we can also try a z-test
diff_in_means = nonwhite_mean - white_mean; diff_in_means
std_norm = sqrt(nonwhite_sd^2/nb + white_sd^2/nw)
z_stat = diff_in_means/std_norm; z_stat
pnorm(z_stat, 0, 1, lower.tail = FALSE)

#let's compute a confidence interval
theta = diff_in_means
sigma_sq = var(all_scores); sigma_sq
conf_lower = theta - 1.96 * (sqrt(sigma_sq))/(sqrt(total)); conf_lower #bounds of conf interval
conf_upper = theta + 1.96 * (sqrt(sigma_sq))/(sqrt(total)); conf_upper
 
#let's do logistic regression on raw score and race
compas$race <- ifelse(compas$Ethnic_Code_Text == "Caucasian", 0, 1)
score <- compas$RawScore
MLL<- function(alpha, beta) {
  -sum( log( exp(alpha+beta*score)/(1+exp(alpha+beta*score)) )*compas$race
        + log(1/(1+exp(alpha+beta*score)))*(1-compas$race) )
}
library(stats4)
plot(score,compas$race)  #not a great candidate for a straight-line approximation, but let's try
b <- cov(score,compas$race)/var(score)    #easy way to get the slope
#Here is the formula for the intercept
a <- mean(compas$race) - b*mean(score);a  
#We can add this regression line to the plot of the data
abline(a, b, col = "red")
#This is bad --- it gives a
results<-mle(MLL, start = list(alpha = 0, beta = 0)) #an initial guess is required
results@coef
curve( exp(results@coef[1]+results@coef[2]*x)/ (1+exp(results@coef[1]+results@coef[2]*x)),col = "blue", add=TRUE)
