setwd('C:/Users/phc07/Desktop/dataHC/01_bigdata/04_PracticalStatistics/Practical_Statistics_DS')
route <- file.path(getwd())

# libraries
library(ggplot2)

# data
data <- read.csv(file.path(route, 'data', 'loans_income.csv'))

# dataframe -> vector
data1 <- data[,1]

# 단순임의표본
df1 <- data.frame(income = sample(data1, 1000),
                  type = 'dist')

# 5개값의 평균
df1_5 <- data.frame(income = tapply(sample(data1, 1000*5),
                                    rep(1:1000, rep(5,1000)),FUN = mean),
                    type = 'mean_of_5')

# 20개값의 평균
df1_20 <- data.frame(income = tapply(sample(data1, 1000*20),
                                    rep(1:1000, rep(20,1000)),FUN = mean),
                    type = 'mean_of_20')

# 바인딩 후 factor로 변환
income <- rbind(df1, df1_5, df1_20)
income$type = factor(income$type,
                     levels = c("dist", "mean_of_5", "mean_of_20"),
                     labels = c("Data", "mean of 5", "mean of 20"))

# 히스토그램 작성
ggplot(income, aes(x=income)) +
  geom_histogram(bins = 40) +
  facet_grid(type ~.)


# Bootstrap_220815
library(boot)

# idx로 지정된 표본의 중앙값 계산
stat_fun <- function(x, idx) median(x[idx])
boot_obj <- boot(data1, R = 1000, statistic=stat_fun)
boot_obj

## Confidence Intervals

# R version for figure 2-9 not available
set.seed(5)
set.seed(7)
sample20 <- sample(data1, 20)
sampleMean <- mean(sample20)

stat_fun <- function(x, idx) mean(x[idx])
boot_obj <- boot(sample20, R=500, statistic=stat_fun)
boot_ci <- boot.ci(boot_obj, conf=0.9, type='basic')
X <- data.frame(mean=boot_obj$t)
ci90 <- boot_ci$basic[4:5]
ci <- data.frame(ci=ci90, y=c(9, 11))
# ci <- boot_ci$basic[4:5]
ci
ggplot(X, aes(x=mean)) +
  geom_histogram(bins=40, fill='#AAAAAA') +
  geom_vline(xintercept=sampleMean, linetype=2) +
  geom_path(aes(x=ci, y=10), data=ci, size=2) +
  geom_path(aes(x=ci90[1], y=y), data=ci, size=2) +
  geom_path(aes(x=ci90[2], y=y), data=ci, size=2) +
  geom_text(aes(x=sampleMean, y=20, label='Sample mean'), size=6) +
  geom_text(aes(x=sampleMean, y=8, label='90% interval'), size=6) +
  theme_bw() + 
  labs(x='', y='Counts')

# 표준정규분포 & QQ plot

nor_sam <- rnorm(100)
qqnorm(nor_sam)
abline(a = 0, b= 1, col = 'green')

# long tail distribution
## load data
sp500 <- read.csv(file.path(route, 'data', 'sp500_data.csv.gz'), row.names=1)

nf <- sp500[, 'NFLX']
nf <- diff(log(nf[nf>0]))
qqnorm(nf)
abline(a=0, b=1, col='red')


## Binomial Distribution

dbinom(x=2, size=5, p=0.1)

pbinom(2, 5, 0.1)

# poisson
rpois(100, lambda = 5)

# exponential
rexp(n =100, rate = 0.05)

# Weibull
rweibull(100, 1.25, 5000)
