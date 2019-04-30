setwd("/Users/shuvomsadhuka/Desktop/College/Freshman/Spring/23b/Project/compas_analysis-master")
compas <- read.csv ("compass/compas-scores-raw.csv"); head(compas)
library(ggplot2)

#messy but there are two clear clusters we can isolate
ggplot(compas, aes(x=RawScore)) + geom_histogram()

#first cluster -> scores < 7 (this is an estimate, 
#should we try using a clustering algorithm like k-means or something?)
scores <- compas[which(compas$RawScore < 7),]; head(scores)
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
stat <- stat_function(fun = dbeta, args = list(param1, param2), lwd = 1, col = "red")
#that looks quite accurate!
plot + stat

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
#dgamma appears to fit!
plot + stat
#do our transformations make the distributions that we fit useless? 

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
