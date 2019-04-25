compas <- read.csv ("compass/compas-scores-raw.csv"); head(compas)

#messy
hist(compas$RawScore, breaks = "FD", probability = TRUE)

#first chunk -> scores < 7
scores <- compas[which(compas$RawScore < 7),]; head(scores1)
max <- max(scores$RawScore)
min <- min(scores$RawScore)
#squeezing the values to be between 0 and 1
score <- (max - scores$RawScore) / (max - min)
hist(score, breaks = "FD", probability = TRUE)
mu <- mean(score); mu
var <- var(score); var
#used Wolfram Alpha to find parameters for a beta distribution by solving a system of equations
param1 <- 15162/3725
param2 <- 11913/3725
#dbeta looks accurate!
curve(dbeta(x, param1, param2), add = TRUE, col = "red")

scores2 <- compas[which(compas$RawScore >= 7),]
hist(scores2$RawScore, breaks = "FD", probability = TRUE)

compas2 <- read.csv("compass/cox-violent-parsed_filt.csv"); head(compas2)
max(stroi(compas2$c_days_from_compas))
compas3 <- read.csv("compass/cox-violent-parsed.csv"); head(compas3)
compas4 <- read.csv("compass/propublica_data_for_fairml.csv"); head(compas4)