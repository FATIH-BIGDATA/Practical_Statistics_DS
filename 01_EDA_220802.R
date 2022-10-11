USA <- read.csv('./data/state.csv')
mean(USA[['Population']])
mean(USA[['Population']], trim = 0.15)
median((USA[['Population']]))

library('matrixStats')
weighted.mean(USA[['Murder.Rate']], w = USA[['Population']])
weightedMedian(USA[['Murder.Rate']], w = USA[['Population']])

# 표준편차
sd(USA[['Population']])
IQR(USA[['Population']])
mad(USA[['Population']])

quantile(USA[['Murder.Rate']], p= c(.05, .25, .5, .75, .95))
boxplot(USA[['Population']] / 1000000, ylab = '인구' )

range <- seq(from = min(USA[['Population']]),
             to = max(USA[['Population']]),
             length = 11)

pop <- cut(USA[['Population']], breaks = range,
           # right : 구간의 오른쪽값 포함 여부
           right = T, 
           # 가장 작은 값이 breaks 포인터와 같을 때, 포함 여부
           include.lowest = T)
table(pop)

hist(USA[['Population']], breaks = range)

hist(USA[['Murder.Rate']], freq = F)
lines(density(USA[['Murder.Rate']]), lwd= 3, col ='skyblue')


# airport delay
airport <- read.csv('./data/dfw_airline.csv')
barplot(as.matrix(airport) / 6, cex.axis = 0.6, cex.names = 0.5,
        xlab = 'Reason of delay', ylab = 'count')

# correlation 
install.packages("corrplot")
library(corrplot)
sp <- read.csv('./data/sp500_data.csv')
sp_s <- read.csv('./data/sp500_sectors.csv')
etf <- sp[row.names(sp) > "2012-07-01", 
          sp_s[sp_s$sector == 'etf','symbol']]
corrplot(cor(etf), method = 'ellipse')

# scatterplot
telecom <- sp[, sp_s[sp_s$sector == 'telecommunications_services', 'symbol']]
telecom <- telecom[row.names(telecom) > '2012-07-01',]
plot(telecom$T, telecom$VZ, xlab = "AT", ylab = 'Verizon')

# hexagonal binning
route <- file.path(getwd())
tax <- read.csv(file.path(route, 'data', 'kc_tax.csv.gz'))

# 극단값 제거
tax <- subset(tax, TaxAssessedValue < 750000 &
                SqFtTotLiving > 100 &
                SqFtTotLiving < 3500)

nrow(tax)

#install.packages("ggplot2")
library(ggplot2)
graph1 <- ggplot(tax, (aes(x = SqFtTotLiving, y = TaxAssessedValue))) +
  stat_binhex(color = 'gray') +
  theme_bw() +
  scale_fill_gradient(low = 'white', high = 'red') +
  labs(x = "집 크기", y = "과세평가금액")
graph1

# contours
graph2 <- ggplot(tax, (aes(SqFtTotLiving, TaxAssessedValue))) +
  theme_bw() +
  geom_point(alpha = 0.07, color = 'red') +
  geom_density2d(color = 'white') +
  labs(x = "집 크기", y = "과세평가금액")
graph2

# 분할표
route <- file.path(getwd())
loan <- read.csv(file.path(route, 'data', 'lc_loans.csv'))

#install.packages("descr")
library(descr)
CrossTable(loan$grade,loan$status,
           prop.c = F, prop.chisq = F, prop.t =F)

# 상자그림
as <- read.csv('./data/airline_stats.csv')

boxplot(pct_carrier_delay ~ airline, data = as, ylim = c(0, 55))

# 바이올린
ggplot(data = as, aes(airline, pct_carrier_delay)) +
  ylim(0, 55) +
  geom_violin() +
  labs(x = '항공사', y = '일일 연착 비율')

# 조건화
ggplot(subset(tax, ZipCode %in% c(98188, 98108, 98107, 98125)),
       aes(SqFtTotLiving, TaxAssessedValue)) +
  stat_binhex(color = 'white') +
  theme_bw() +
  scale_fill_gradient(low = 'white', high = 'purple') +
  labs(x = '집 크기', y=  '과세평가금액') +
  facet_wrap('ZipCode')

