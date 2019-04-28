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
