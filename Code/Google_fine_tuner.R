##### HEAD #####
rm(list=ls(all=TRUE))
setwd("C:/Users/szv6w1/Documents/Wombat")

##### BIBLIOTEKA #####
library(vars)
library(forecast)
library(tseries)
library(urca)
library(Hmisc)
library(mFilter)
library(gtrendsR)
library(spikeslab)
library(bsts)
library(lubridate)
library(plyr)
library(parallel)
library(pryr)
library(magrittr)
library(doParallel)
library(iterators)
library(foreach)
library(tsDyn)
library(nnet)
library(reshape)

### Licz
dane_gg<-ts(log(read.csv("./Dane/GDane.csv",sep=";")[,-1]),start=c(2004,1),freq=12)
dane_gg[dane_gg==-Inf]<-0
for (ig in 1:ncol(dane_gg)){
	dane_gg[,ig]<-dane_gg[,ig]-stl(dane_gg[,ig],s.window="periodic")$time.series[,1]
	dane_gg[,ig]<-hpfilter(ts(dane_gg[,ig],freq=12),type="lambda",freq=1)$trend
}

dane_gg<-cbind(dane_gg,lag(dane_gg,-1),lag(dane_gg,-2),lag(dane_gg,-3),lag(dane_gg,-4),lag(dane_gg,-5),lag(dane_gg,-6))
dane_gg<-diff(dane_gg)
dane_gg<-window(dane_gg,c(2007,4),c(2016,12))
plot(dane_gg[,1:6])

ORDER_ARMA<-array(NA,dim=c(ncol(dane_gg)/7,57))

for (t in 1:57){
	for(i in 1:(ncol(dane_gg)/7)){
		ord<-auto.arima(ts(dane_gg[(t):(t+59),i],freq=12),max.p=2,max.q=2,max.P=2,max.Q=2)$arma[c(1,6,2)]
		ORDER_ARMA[i,t]<-ord[1]*100+ord[2]*10+ord[3]
	}
}

as.matrix(apply(ORDER_ARMA,1,function(x){names(which.max(table(x)))}))