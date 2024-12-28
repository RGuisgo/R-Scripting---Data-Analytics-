# --- PRE-PROCESSING AND PRELIMINARY TESTS ---
rm(list=ls()) 
#install.packages("AER")
library(AER)
install.packages('dLagM')
install.packages('tictoc')
install.packages('lmtest')
install.packages('tseries')
install.packages('forecast')
install.packages('pracma')
install.packages('egcm')
install.packages('urca')
install.packages('dynamac')
install.packages('readxl')
install.packages('ARDL')
install.packages('dynlm')
install.packages('ggplot2')
install.packages('xtable')
install.packages('stargazer')
library(stargazer)
library(xtable)
library(dLagM)
library(tictoc)
library(lmtest)
library(tseries)
library(forecast)
library(pracma)
library(egcm)
library(urca)
library(dynamac)
library(readxl)
library(ARDL)
library(tseries)
library(dynlm)
library(ggplot2)


# setting a working directory

setwd("C:/Users/User/Desktop/Documents/Mine/corvinus/thesis")


#  data

my_data <- read_excel("NewData.xlsx")
head(my_data)
tail(my_data)

boxplot(my_data)

inflation = my_data$Inflation
exchange = my_data$Real_effective_exchange
fdi = my_data$FDI
gdp = my_data$GDP
trade = my_data$Trade
debt = my_data$External_debt



### Plotting
par(mfcol = c(2,4))
plot(inflation, col='black', lwd = 2, type='l')
plot(exchange, col='black', lwd = 2, type='l')
plot(fdi, col='black', lwd = 2, type='l')
plot(gdp, col='black', lwd = 2, type='l')
plot(trade, col='black', lwd = 2, type='l')
plot(debt, col='black', lwd = 2, type='l')
plot(resource, col='black', lwd = 2, type='l')




## test for stationarity

############################################### 
#> # Augmented Dickey-Fuller Test Unit Root Test # 
#> ############################################### 
#
adf.test(my_data$FDI)
adf.test(my_data$Inflation)
adf.test(my_data$Real_effective_exchange)
adf.test(my_data$GDP)
adf.test(my_data$Trade)
adf.test(my_data$External_debt)



summary(ur.df(my_data$FDI, type = c("trend"),lags = 5, selectlags = "AIC"))# non stationary
summary(ur.df(my_data$Inflation, type = c("trend"), lags = 5,selectlags = "AIC"))# stationary
summary(ur.df(my_data$Real_effective_exchange, type = c("trend"), lags = 5,selectlags = "AIC"))# stationary
summary(ur.df(my_data$GDP, type = c("trend"), lags = 1,selectlags = "AIC"))#non stationary
summary(ur.df(my_data$Trade, type = c("trend"), lags = 1,selectlags = "AIC"))# non stationary
summary(ur.df(my_data$External_debt, type = c("trend"), lags = 1,selectlags = "AIC"))#non stationary




################################# 
#> # Phillips-Perron Unit Root Test # 
#> ################################## 
pp.test(my_data$FDI)
pp.test(my_data$Inflation)
pp.test(my_data$Real_effective_exchange)
pp.test(my_data$GDP)
pp.test(my_data$Trade)
pp.test(my_data$External_debt)


### At levels


summary(ur.pp(my_data$FDI, type = c("Z-tau"), model = c("constant"), use.lag = 5))#non stationary
summary(ur.pp(my_data$Inflation, type = c("Z-tau"), model = c("constant"), use.lag = 1))# stationary
summary(ur.pp(my_data$Real_effective_exchange, type = c("Z-tau"), model = c("constant"), use.lag = 5))# non stationary
summary(ur.pp(my_data$GDP, type = c("Z-tau"), model = c("constant"), use.lag = 5))# stationary
summary(ur.pp(my_data$Trade, type = c("Z-tau"), model = c("constant"), use.lag = 5))#non stationary
summary(ur.pp(my_data$External_debt, type = c("Z-tau"), model = c("constant"), use.lag = 5))#non stationary



### At levels with trend

summary(ur.pp(my_data$FDI, type = c("Z-tau"), model = c("trend"),use.lag = 5))#non stationary
summary(ur.pp(my_data$Inflation, type = c("Z-tau"), model = c("trend"), use.lag = 5))# stationary
summary(ur.pp(my_data$Real_effective_exchange, type = c("Z-tau"), model = c("trend"), use.lag = 1))# non stationary
summary(ur.pp(my_data$GDP, type = c("Z-tau"), model = c("trend"), use.lag = 1))# stationary
summary(ur.pp(my_data$Trade, type = c("Z-tau"), model = c("trend"), use.lag = 1))#non stationary
summary(ur.pp(my_data$External_debt, type = c("Z-tau"), model = c("trend"), use.lag = 1))#non stationary





##At first difference with no trend

summary(ur.pp(diff(my_data$FDI), type = c("Z-tau"), model = c("constant"), use.lag = 0))#non stationary
summary(ur.pp(diff(my_data$Inflation), type = c("Z-tau"), model = c("constant"), use.lag = 1))# stationary
summary(ur.pp(diff(my_data$Real_effective_exchange), type = c("Z-tau"), model = c("constant"), use.lag = 1))# non stationary
summary(ur.pp(diff(my_data$GDP), type = c("Z-tau"), model = c("constant"), use.lag = 1))# stationary
summary(ur.pp(diff(my_data$Trade), type = c("Z-tau"), model = c("constant"), use.lag = 1))#non stationary
summary(ur.pp(diff(my_data$External_debt), type = c("Z-tau"), model = c("constant"), use.lag = 1))#non stationary





####################### 
#> # KPSS Unit Root Test # 
#> ####################### 
kpss.test(my_data$FDI,null = 'Trend')
kpss.test(my_data$Inflation)
kpss.test(my_data$Real_effective_exchange)
kpss.test(my_data$GDP)
kpss.test(my_data$Trade)
kpss.test(my_data$External_debt)
kpss.test(my_data$natural_resources)

summary(ur.kpss(my_data$FDI, type = c("tau"), use.lag = 5))#non stationary
summary(ur.kpss(my_data$Inflation, type = c("tau"), use.lag = 5))# stationary
summary(ur.kpss(my_data$Real_effective_exchange, type = c("tau"), use.lag = 5))#stationary
summary(ur.kpss(my_data$GDP, type = c("tau"), use.lag = 5))#stationary
summary(ur.kpss(my_data$Trade, type = c("tau"), use.lag = 5))#non stationary
summary(ur.kpss(my_data$External_debt, type = c("tau"), use.lag = 5))#non stationary




##differencing

############################################### 
#> # Augmented Dickey-Fuller Test Unit Root Test # 
#> ############################################### 
#
summary(ur.df(diff(my_data$FDI), type = c("trend"),selectlags = "AIC"))#  stationary
summary(ur.df(diff(my_data$Inflation), type = c("trend"), lags = 1,selectlags = "AIC"))# stationary
summary(ur.df(diff(my_data$Real_effective_exchange), type = c("trend"), lags = 1,selectlags = "AIC"))# stationary
summary(ur.df(diff(my_data$GDP), type = c("trend"), lags = 1,selectlags = "AIC"))#stationary
summary(ur.df(diff(my_data$Trade), type = c("trend"), lags = 1,selectlags = "AIC"))#stationary
summary(ur.df(diff(my_data$External_debt), type = c("trend"), lags = 0,selectlags = "AIC"))#stationary



################################# 
#> # Phillips-Perron Unit Root Test # 
#> ################################## 

pp.test(diff(my_data$FDI))
pp.test(diff(my_data$Inflation))
pp.test(diff(my_data$Real_effective_exchange))
pp.test(diff(my_data$GDP))
pp.test(diff(my_data$Trade))
pp.test(diff(my_data$External_debt))
pp.test(diff(my_data$External_debt))

summary(ur.pp(diff(my_data$FDI), type = c("Z-tau"), model = c("trend"), use.lag = 1))#stationary
summary(ur.pp(diff(my_data$Inflation), type = c("Z-tau"), model = c("trend"), use.lag = 1))# stationary
summary(ur.pp(diff(my_data$Real_effective_exchange), type = c("Z-tau"), model = c("trend"), use.lag = 1))#stationary
summary(ur.pp(diff(my_data$GDP), type = c("Z-tau"), model = c("trend"), use.lag = 1))# stationary
summary(ur.pp(diff(my_data$Trade), type = c("Z-tau"), model = c("trend"), use.lag = 1))#stationary
summary(ur.pp(diff(my_data$External_debt), type = c("Z-tau"), model = c("trend"), use.lag = 1))#stationary



####################### 
#> # KPSS Unit Root Test # 
#> ####################### 

summary(ur.kpss(diff(my_data$FDI), type = c("mu"), use.lag = 1))# stationary
summary(ur.kpss(diff(my_data$Inflation), type = c("mu"), use.lag = 1))# stationary
summary(ur.kpss(diff(my_data$Real_effective_exchange), type = c("mu"), use.lag = 1))# stationary
summary(ur.kpss(diff(my_data$GDP), type = c("mu"), use.lag = 1))#stationary
summary(ur.kpss(diff(my_data$Trade), type = c("mu"), use.lag = 1))#stationary
summary(ur.kpss(diff(my_data$External_debt), type = c("mu"), use.lag = 1))# stationary




## GRANGER CAUSALTY TEST


#fdi

grangertest(fdi~gdp,order=3)
grangertest(fdi~trade,order=3)
grangertest(fdi~exchange,order=1)
grangertest(fdi~debt,order=2)
grangertest(fdi~inflation,order=2)

#gdp

grangertest(gdp~inflation,order=2)
grangertest(gdp~fdi,order=2)
grangertest(gdp~trade,order=2)
grangertest(gdp~debt,order=2)
grangertest(gdp~exchange,order=2)


# debt

grangertest(debt~inflation,order=2)
grangertest(debt~fdi,order=2)
grangertest(debt~trade,order=2)
grangertest(debt~gdp,order=2)
grangertest(debt~exchange,order=3)

#trade

grangertest(trade~inflation,order=2)
grangertest(trade~fdi,order=2)
grangertest(trade~debt,order=2)
grangertest(trade~gdp,order=2)
grangertest(trade~exchange,order=2)


#exchange

grangertest(exchange~inflation,order=2)
grangertest(exchange~fdi,order=2)
grangertest(exchange~debt,order=2)
grangertest(exchange~gdp,order=2)
grangertest(exchange~trade,order=2)


#inflation 

grangertest(inflation~exchange,order=2)
grangertest(inflation~fdi,order=2)
grangertest(inflation~debt,order=3)
grangertest(inflation~gdp,order=2)
grangertest(inflation~trade,order=2)




# --- PRE-PROCESSING AND PRELIMINARY TESTS ---

############################################################################
##                                                                        ##
## dLagM: An R package for distributed lag models and ARDL bounds testing ##
##                                                                        ##
##                                                                        ##
############################################################################



##############################################################################
##                                                                          ##
## Computes optimal orders (lag structure) for the short-run relationships  ##
## and autoregressive part of the ARDL model prior to ARDL bounds test with ##
## the approach of Pesaran et al. (2001).                                   ##
##                                                                          ##
##############################################################################


##ARDL-bounds is to estimate the error-correction form of the model.

formula = FDI ~ GDP  + Real_effective_exchange + External_debt + Trade + Inflation
tic("Standard")
orders<- ardlBoundOrders(data = my_data, formula = formula, ic = "AIC", max.p = 3,  max.q = 3, FullSearch = FALSE)
toc()
lags = orders$p
orders$q
orders$min.Stat
p <- data.frame(orders$q , lags )  # plus 1 because the formula equation willl start from zero
test <- ardlBound(data = my_data, formula = formula, case = 2, autoOrder = TRUE ,ic = "AIC",max.p= 3, max.q= 3,ECM = TRUE,stability = TRUE)
summary(test$model$modelFull)# for significance test






