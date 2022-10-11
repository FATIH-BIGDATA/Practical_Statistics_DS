# libraries
library(ggplot2)


# load data

setwd('C:/Users/phc07/Desktop/dataHC/01_bigdata/04_PracticalStatistics/Practical_Statistics_DS')
route <- file.path(getwd())

session <- read.csv(file.path(route, 'data', 'web_page_data.csv'))
session[,2] <- session[,2] * 100

ggplot(session, aes(x = Page, y = Time)) + 
  geom_boxplot()

## average

mean_a <- mean(session[session['Page'] == 'Page A', 'Time'])
mean_b <- mean(session[session['Page'] == 'Page B', 'Time'])
mean_b - mean_a

## Permutation test
per <- function(x, A, B)
{
  n<- A + B
  idx_b <- sample(1:n, B)
  idx_a <- setdiff(1:n, idx_b)
  mean_diff <- mean(x[idx_b]) - mean(x[idx_a])
  return(mean_diff)
}

perm <- rep(0,1000) 
for (i in 1:1000){
  perm[i] = per(session[,'Time'], 21, 15)
}
hist(perm, xlab = 'Sessiong Time Diff')
abline(v = mean_b - mean_a)
