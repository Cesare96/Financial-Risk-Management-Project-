#### Exercise 1.1 ####

data = read.csv('FinancialRisk.csv', sep = ';', header = T)
data = na.omit(data)
data$Date = as.Date(data$Date, format = '%d-%b-%y')

data$logClose = log(data$Close, exp(1))
logClose = data$logClose
logReturn = diff(logClose)
logReturn = c(NaN, logReturn)
data$logReturn = logReturn 

ggplot(data, aes(x = Date, y = Close)) +
  geom_line(color ='#119dec') +
  ggtitle('Close Prices S&P 500')

ggplot(data, aes(x = Date, y = logReturn)) +
  geom_line(color = '#119dec') +
  ggtitle('LogReturns of S&P 500')

#### Exercise 1.3 ####

Correlation = c()

x = ggAcf(data$logReturn, plot = F, lag.max = 100)
names(x)  
length(x$acf)
Correlation = x$acf[-1]
length(Correlation) #different values because R calculates the correct correlation coefficient with T-1 degrees of freedom
ggAcf(data$logReturn, lag.max = 100)

plot(Correlation, type = 'l', col = '4', lwd = '2.5', xlab = 'Lag', ylab = 'ACF', main = 'ACF')

#### Exercise 1.5 ####

sigma0 = var(data$logReturn, na.rm = T)
lambda = 0.94
data$Conditional_Variance = NaN
data$Conditional_Variance[2] = sigma0
i = 1:1524

for (i in 3:2514){
  data$Conditional_Variance[i] = lambda * data$Conditional_Variance[i-1] + (1- lambda) * (data$logReturn[i-1])^2
}

data$Conditional_Std_Deviation = sqrt(data$Conditional_Variance)

ggplot(data, aes(x = Date, y = Conditional_Std_Deviation)) +
  geom_line(color = '#119dec', linetype = 1, linewidth = 0.8) +
  ggtitle('Conditional Standard Deviation')

#### Exercise 2.1 ####

data2 = read.csv('FinancialChapter2.csv', header = T, sep = ';')
data2$Date = as.Date(data2$Date, format = '%d-%b-%y')

ggplot(data2, aes(x = Date)) +
  geom_line(aes(y = logReturn), color = '#119dec', linetype = 1, linewidth = 1.4) +
  geom_line(aes(y = VaR), color = '#bd1a1a', linetype = 1, linewidth = 1.4) +
  ggtitle('Historical Simulation VaR  and Daily Losses form Short S&P 500 Position, October 1987') +
  xlab('Time') +
  ylab('HS VaR and Loss')

ggplot(data2, aes(x = Date)) +
  geom_line(aes(y = MinusLogReturn), color = '#119dec', linetype = 1, linewidth = 1.4) +
  geom_line(aes(y = VaR), color = '#bd1a1a', linetype = 1, linewidth = 1.4) +
  ggtitle('Historical Simulation VaR  and Daily Losses form Long S&P 500 Position, October 1987') +
  xlab('Time') +
  ylab('HS VaR and Loss')




#### Exercise 4.1 ####

df = read.csv('Garch_ex_Financial.csv', header = T, sep = ';')

df$Return2 = df$Return^2
df$sigma2_t = NaN
df$sigma2_t[2] = var(df$Return, na.rm = T)
alpha = 0.10
beta = 0.85
omega = df$sigma2_t[1]*(1- alpha - beta)




logReturn = na.omit(df$Return)
adf.test(logReturn)
ArchTest(logReturn)

garch(logReturn, grad = 'numeric', trace = F)
garchmodel = ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
garchfit = ugarchfit(garchmodel, data = logReturn)
garchfit
coef(garchfit)



alpha.est = 7.996574e-02
beta.est = 9.112903e-01 
omega.est = 1.262318e-06

persistence = alpha.est + beta.est
persistence < 1

for(i in 3:nrow(df)){
  df$sigma2_t[i] = omega.est+alpha.est*df$Return2[i-1]+beta.est*df$sigma2_t[i-1]
}

df$LogLikelihood = NaN
for (i in 2:nrow(df)){
  df$LogLikelihood[i] = (-1/2)*(log(2*pi, exp(1))+log(df$sigma2_t[i], exp(1))+ df$Return2[i]/df$sigma2_t[i])
  }

sumLogLikelihood = sum(df$LogLikelihood, na.rm = T)


plot(garchfit)
