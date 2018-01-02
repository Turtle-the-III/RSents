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
library(spectral)


OUT_PROGNOZA<-NULL
NEW_PROGNOZA<-list(prognoza=NULL)

##### PARAMETRY #####
sob<-0
pocz<-as.Date("01-01-2007","%d-%m-%Y")
p_lag<-2
assign("p_lag",p_lag,envir = .GlobalEnv)

Regimes<-list(3:20,21:38,39:57)
Regimes<-lapply(Regimes,function(x){return(x-2)})

source("./holidays.R")
source("./PrivateLib.R")
source("./DataPrep.R")

dev.new()
plot_senty(dane_var,60)
lines(-sent,col="red",lwd=2)
abline(v=2012+(2/12)+(min(Regimes[[1]]+2)/12),col="blue",lty=4)
abline(v=2012+(2/12)+(min(Regimes[[2]]+2)/12),col="blue",lty=4)
abline(v=2012+(2/12)+(min(Regimes[[3]]+2)/12),col="blue",lty=4)
#abline(v=59+26,col="blue")
#abline(v=59+37,col="blue")
#abline(v=59+47,col="blue")
legend("top",legend=c("Rolling-window","Whole sample"),col=c("black","red"), pch =15,bg="white",horiz=TRUE,xpd=TRUE, cex = 1.2)
dev.print(pdf,"Senty.pdf")

dev.new()
#par(mfrow=c(1,3))
temp_sent<-get_sents(dane_var[,-c(3,6,7)])$sent
temp_sent_oseas<-stl(ts(temp_sent,freq=9),s.window="periodic")$time.series[,2]
hype<-ts(stl(ts(temp_sent,freq=9),s.window="periodic")$time.series[,1],start=start(temp_sent),freq=12)
sent_print<-ts(-cumsum(temp_sent),start=start(temp_sent),freq=12)
sent_print_oseas<-ts(-cumsum(temp_sent_oseas),start=start(temp_sent),freq=12)
temp_wwuk<-hpfilter(wwuk,freq=1)$trend
wwuk_print<-window(temp_wwuk,start=start(temp_sent),end=end(temp_sent))
wwuk_print_oseas<-stl(ts(c(diff(wwuk_print)),freq=8),s.window="periodic")$time.series[,2]
wwuk_print_oseas<-ts(cumsum(wwuk_print_oseas),start=start(temp_sent),freq=12)
hype_wwuk<-ts(stl(ts(diff(wwuk_print),freq=8),s.window="periodic")$time.series[,1],start=start(temp_sent),freq=12)
#plot(sent_print,type="l")
#plot(wwuk_print)
plot((wwuk_print-mean(wwuk_print))/sd(wwuk_print),ylim=c(-2,3.5),lwd=2,ylab="Normalized values")
lines((sent_print-mean(sent_print))/sd(sent_print),col="red",lwd=2)
lines((sent_print_oseas-mean(sent_print_oseas))/sd(sent_print_oseas),col="blue",lwd=2,lty=4)
lines((sent_print_oseas-mean(sent_print_oseas))/sd(sent_print_oseas),col="blue",lwd=2,lty=4)
#lines((wwuk_print_oseas-mean(wwuk_print_oseas))/sd(wwuk_print_oseas),col="black",lwd=2,lty=4)

cor(sent_print,wwuk_print)
cor(diff(sent_print),diff(wwuk_print))
cor(sent_print_oseas,wwuk_print)
cor(diff(sent_print_oseas),diff(wwuk_print))
#abline(v=59+(3/12),col="blue")
#abline(v=59+18,col="blue")
#abline(v=59+26,col="blue")
#abline(v=59+37,col="blue")
#abline(v=59+47,col="blue")
legend("top",legend=c("Smoothed WWUK","Extracted sentiments","Unhyped sentiments"),col=c("black","red","blue"), pch =15,bg="white",horiz=FALSE,xpd=TRUE, cex = 1.2)
dev.print(pdf,"WWUK.pdf")

dev.new()
x.spec <- spectrum(sent,span=12,log="no",plot=FALSE)
spx <- x.spec$freq/(1/10)
spy <- 2*x.spec$spec
plot(spy~spx,xlab="frequency",ylab="density",main="smoothed spectral density",type="l")
abline(v=which.max(spy),col="blue",lty=4)
dev.print(pdf,"Spectral.pdf")


rm(sent_print,wwuk_print)

#3:20
#21:38
#39:57

okres<-3:57
	for (i in okres){
		OUT_PROGNOZA<-rbind(OUT_PROGNOZA,c(read.csv(paste("./Temp/Forecast",i,".csv",sep=""))))
	}

NEW_PROGNOZA$prognoza<-OUT_PROGNOZA[,-1]

NEW_PROGNOZA$prognoza<-cbind(
	NEW_PROGNOZA$prognoza,
	c(tail(dane_var[,2],55))[okres-2],
	c(tail(dane_var[,2],53),rep(NA,2))[okres-2],
	c(tail(dane_var[,2],50),rep(NA,5))[okres-2],
	c(tail(dane_var[,2],44),rep(NA,11))[okres-2]
)
#NEW_PROGNOZA$prognoza[2,c(283:286)]<-NEW_PROGNOZA$prognoza[1,c(283:286)]
hor<-3:ncol(NEW_PROGNOZA$prognoza)

NEW_PROGNOZA_1M<-NEW_PROGNOZA$prognoza[,c(1,2,hor[(hor %% 4)==3])]
NEW_PROGNOZA_3M<-NEW_PROGNOZA$prognoza[,c(1,2,hor[(hor %% 4)==0])]
NEW_PROGNOZA_6M<-NEW_PROGNOZA$prognoza[,c(1,2,hor[(hor %% 4)==1])]
NEW_PROGNOZA_12M<-NEW_PROGNOZA$prognoza[,c(1,2,hor[(hor %% 4)==2])]

NEW_PROGNOZA_1M<-NEW_PROGNOZA_1M[!is.na(sapply(NEW_PROGNOZA_1M[,ncol(NEW_PROGNOZA_1M)],function(x){x})),]
NEW_PROGNOZA_3M<-NEW_PROGNOZA_3M[!is.na(sapply(NEW_PROGNOZA_3M[,ncol(NEW_PROGNOZA_3M)],function(x){x})),]
NEW_PROGNOZA_6M<-NEW_PROGNOZA_6M[!is.na(sapply(NEW_PROGNOZA_6M[,ncol(NEW_PROGNOZA_6M)],function(x){x})),]
NEW_PROGNOZA_12M<-NEW_PROGNOZA_12M[!is.na(sapply(NEW_PROGNOZA_12M[,ncol(NEW_PROGNOZA_12M)],function(x){x})),]

#NEW_PROGNOZA_12M<-NEW_PROGNOZA_1M
NAZWY_PROGNOZA<-NEW_PROGNOZA$kwerendy
#rm(NEW_PROGNOZA)

NEW_PROGNOZA_1M<-apply(NEW_PROGNOZA_1M,2,as.numeric)
NEW_PROGNOZA_3M<-apply(NEW_PROGNOZA_3M,2,as.numeric)
NEW_PROGNOZA_6M<-apply(NEW_PROGNOZA_6M,2,as.numeric)
NEW_PROGNOZA_12M<-apply(NEW_PROGNOZA_12M,2,as.numeric)

blad_1M_new<-NEW_PROGNOZA_1M[,-ncol(NEW_PROGNOZA_1M)]-NEW_PROGNOZA_1M[,ncol(NEW_PROGNOZA_1M)]
blad_3M_new<-NEW_PROGNOZA_3M[,-ncol(NEW_PROGNOZA_3M)]-NEW_PROGNOZA_3M[,ncol(NEW_PROGNOZA_3M)]
blad_6M_new<-NEW_PROGNOZA_6M[,-ncol(NEW_PROGNOZA_6M)]-NEW_PROGNOZA_6M[,ncol(NEW_PROGNOZA_6M)]
blad_12M_new<-NEW_PROGNOZA_12M[,-ncol(NEW_PROGNOZA_12M)]-NEW_PROGNOZA_12M[,ncol(NEW_PROGNOZA_12M)]

new_blad<-cbind(
	apply(blad_1M_new[,],2,rmse),
	apply(blad_3M_new[,],2,rmse),
	apply(blad_6M_new[,],2,rmse),
	apply(blad_12M_new[,],2,rmse)
)


##### RESULTS #####
colnames(new_blad)<-c("1M","3M","6M","12M")
rownames(new_blad)<-c("naive","RW",
			"ARIMA_00","ARIMA_10","ARIMA_20","ARIMA_01","ARIMA_11","ARIMA_21","ARIMA_02","ARIMA_12","ARIMA_22",
			"ARIMAXS_00","ARIMAXS_10","ARIMAXS_20","ARIMAXS_01","ARIMAXS_11","ARIMAXS_21","ARIMAXS_02","ARIMAXS_12","ARIMAXS_22",
			"ARIMAXW_00","ARIMAXW_10","ARIMAXW_20","ARIMAXW_01","ARIMAXW_11","ARIMAXW_21","ARIMAXW_02","ARIMAXW_12","ARIMAXW_22",
			"LSTAR_11","LSTAR_21","LSTAR_21","LSTAR_22",
			"ARIMAXGT_00","ARIMAXGT_10","ARIMAXGT_20","ARIMAXGT_01","ARIMAXGT_11","ARIMAXGT_21","ARIMAXGT_02","ARIMAXGT_12","ARIMAXGT_22",
			"ARNNETGT_00","ARNNETGT_10","ARNNETGT_20","ARNNETGT_01","ARNNETGT_11","ARNNETGT_21","ARNNETGT_02","ARNNETGT_12","ARNNETGT_22",
			"NNET_00","NNET_10","NNET_20","NNET_01","NNET_11","NNET_21","NNET_02","NNET_12","NNET_22",
			"NNETGT_00","NNETGT_10","NNETGT_20","NNETGT_01","NNETGT_11","NNETGT_21","NNETGT_02","NNETGT_12","NNETT_22",
			"VAR_1","VAR_2","VAR_3"
	)

rownames(new_blad)<-paste(1:nrow(new_blad),rownames(new_blad),sep="_")

### In respect to RW
### BEST among the BEST
(zzz<-t(apply(new_blad,1,function(x){x*100/new_blad[2,]})))
rbind(
	c("Naive",round(zzz[1,],2)),
	c("RW",round(zzz[2,],2)),
	c("ARMA",apply(zzz[3:11,],2,function(x){round(min(x),2)})),
	c("ARMAXS",apply(zzz[12:20,],2,function(x){round(min(x),2)})),
	c("ARMAXW",apply(zzz[21:29,],2,function(x){round(min(x),2)})),
	c("LSTAR",apply(zzz[30:33,],2,function(x){round(min(x),2)})),
	c("LSTARWWUK",apply(zzz[34:37,],2,function(x){round(min(x),2)})),
	c("ARMAXGT",apply(zzz[38:46,],2,function(x){round(min(x),2)})),
	c("ARNNETGT",apply(zzz[47:55,],2,function(x){round(min(x),2)})),
	c("NNET",apply(zzz[56:64,],2,function(x){round(min(x),2)})),
	c("NNETGT",apply(zzz[65:73,],2,function(x){round(min(x),2)})),
	c("NNETGTMEAN",apply(zzz[74:82,],2,function(x){round(min(x),2)})),
	c("VAR",apply(zzz[83:85,],2,function(x){round(min(x),2)}))
)

rbind(
	c("ARMA",round(apply(zzz[3:11,],2,min)/apply(zzz[3:11,],2,min)*100,2)),
	c("ARMAXS",round(apply(zzz[12:20,],2,min)/apply(zzz[3:11,],2,min)*100,2)),
	c("ARMAXW",round(apply(zzz[21:29,],2,min)/apply(zzz[3:11,],2,min)*100,2)),
	c("LSTAR",round(apply(zzz[30:33,],2,min)/apply(zzz[3:11,],2,min)*100,2)),
	c("LSTARWWUK",round(apply(zzz[34:37,],2,min)/apply(zzz[3:11,],2,min)*100,2)),
	c("ARMAXGT",round(apply(zzz[38:46,],2,min)/apply(zzz[3:11,],2,min)*100,2)),
	c("ARNNETGT",round(apply(zzz[47:55,],2,min)/apply(zzz[3:11,],2,min)*100,2)),
	c("NNET",round(apply(zzz[56:64,],2,min)/apply(zzz[3:11,],2,min)*100,2)),
	c("NNETGT",round(apply(zzz[65:73,],2,min)/apply(zzz[3:11,],2,min)*100,2)),
	c("NNETGTMEAN",round(apply(zzz[74:82,],2,min)/apply(zzz[3:11,],2,min)*100,2)),
	c("VAR",round(apply(zzz[83:85,],2,min)/apply(zzz[3:11,],2,min)*100,2))
)

round(apply(zzz[43:51,],2,max)/apply(zzz[3:11,],2,min),2)
round(apply(zzz[61:69,],2,max)/apply(zzz[3:11,],2,min),2)
(aa<-100*apply(cbind(blad_1M_new[,3:11],blad_3M_new[,3:11],blad_6M_new[,3:11],blad_12M_new[,3:11]),1,rmse))
(ab<-100*apply(cbind(blad_1M_new[,43:51],blad_3M_new[,43:51],blad_6M_new[,43:51],blad_12M_new[,43:51]),1,rmse))
(ac<-100*apply(cbind(blad_1M_new[,61:69],blad_3M_new[,61:69],blad_6M_new[,61:69],blad_12M_new[,61:69]),1,rmse))
(ad<-100*apply(cbind(blad_1M_new[,34:42],blad_3M_new[,34:42],blad_6M_new[,34:42],blad_12M_new[,34:42]),1,rmse))
which(ab/aa>1)+2
which(ac/aa>1)+2
which(ad/aa>1)+2
order(-aa[which(ab/aa>1)])+2


arrnet12m<-blad_12M_new[,c(43:51)]
arma12m<-blad_12M_new[,c(3:11)]
arrnet6m<-blad_6M_new[,c(43:51)]
arma6m<-blad_6M_new[,c(3:11)]
arrnet3m<-blad_3M_new[,c(43:51)]
arma3m<-blad_3M_new[,c(3:11)]
arrnet1m<-blad_1M_new[,c(43:51)]
arma1m<-blad_1M_new[,c(3:11)]


Sprawdz<-list(3:11,12:20,21:29,30:33,34:37,38:46,47:55,56:64,65:73,74:82,83:85)
BESTY<-array(NA,dim=c(length(Sprawdz),4))

for (i in 1:length(Sprawdz)){
	BESTY[i,]<-c(
		which.min(apply(blad_1M_new[,Sprawdz[[i]]],2,rmse)),
		which.min(apply(blad_3M_new[,Sprawdz[[i]]],2,rmse)),
		which.min(apply(blad_6M_new[,Sprawdz[[i]]],2,rmse)),
		which.min(apply(blad_12M_new[,Sprawdz[[i]]],2,rmse))
	)
}
rownames(BESTY)<-	c("ARMA","ARMAXS","ARMAXW","LSTAR","LSTARWWUK","ARMAXGT","ARNNETGT","NNET","NNETGT","NNETGTMEAN","VAR")
colnames(BESTY)<-c("1M","3M","6M","12M")

source("./BestInRegime.R")
source("./BestBledy.R")
source("./FinalResults.R")

dev.new()
par(mfrow=c(2,2))
hist(best_bledy_arrnet[!is.na(best_bledy_arrnet[,4]),4],
	breaks=seq(min(best_bledy_arrnet[!is.na(best_bledy_arrnet[,4]),4]),max(best_bledy_arrnet[!is.na(best_bledy_arrnet[,4]),4]),length.out=16),main="12M")
hist(best_bledy_arrnet[!is.na(best_bledy_arrnet[,3]),3],
	breaks=seq(min(best_bledy_arrnet[!is.na(best_bledy_arrnet[,3]),3]),max(best_bledy_arrnet[!is.na(best_bledy_arrnet[,3]),3]),length.out=16),main="6M")
hist(best_bledy_arrnet[!is.na(best_bledy_arrnet[,2]),2],
	breaks=seq(min(best_bledy_arrnet[!is.na(best_bledy_arrnet[,2]),2]),max(best_bledy_arrnet[!is.na(best_bledy_arrnet[,2]),2]),length.out=16),main="3M")
hist(best_bledy_arrnet[!is.na(best_bledy_arrnet[,1]),1],
	breaks=seq(min(best_bledy_arrnet[!is.na(best_bledy_arrnet[,1]),1]),max(best_bledy_arrnet[!is.na(best_bledy_arrnet[,1]),1]),length.out=16),main="1M")

dev.new()
par(mfrow=c(2,2))
hist(best_bledy_arma[!is.na(best_bledy_arma[,4]),4],
	breaks=seq(min(best_bledy_arma[!is.na(best_bledy_arma[,4]),4]),max(best_bledy_arma[!is.na(best_bledy_arma[,4]),4]),length.out=16),main="12M")
hist(best_bledy_arma[!is.na(best_bledy_arma[,3]),3],
	breaks=seq(min(best_bledy_arma[!is.na(best_bledy_arma[,3]),3]),max(best_bledy_arma[!is.na(best_bledy_arma[,3]),3]),length.out=16),main="6M")
hist(best_bledy_arma[!is.na(best_bledy_arma[,2]),2],
	breaks=seq(min(best_bledy_arma[!is.na(best_bledy_arma[,2]),2]),max(best_bledy_arma[!is.na(best_bledy_arma[,2]),2]),length.out=16),main="3M")
hist(best_bledy_arma[!is.na(best_bledy_arma[,1]),1],
	breaks=seq(min(best_bledy_arma[!is.na(best_bledy_arma[,1]),1]),max(best_bledy_arma[!is.na(best_bledy_arma[,1]),1]),length.out=16),main="1M")

NAZWY<-list(NULL)
NAZWY_print<-matrix(0,nrow=ncol(dane_gg),ncol=length(3:57))
rownames(NAZWY_print)<-colnames(dane_gg)

persystencja<-NULL
for (p in okres-2){
	NAZWY[[p]]<-as.character(read.table(paste("./TempTXT/Kwerendy_",p+2,".txt",sep=""))[,1])
	NAZWY_print[,p]<-as.numeric(colnames(dane_gg) %in% NAZWY[[p]])
	
	if(p!=1){
		persystencja<-c(persystencja,sum(NAZWY_print[,p-1]+NAZWY_print[,p]==2)/sum(NAZWY_print[,p-1]+NAZWY_print[,p]>0))	
	}
}

apply(NAZWY_print,2,sum)

popularne<-apply(NAZWY_print,1,sum)
popularne<-popularne[popularne>0]
popularne<-popularne[order(-popularne)]
head(popularne,15)

Np<-NAZWY_print[rownames(NAZWY_print) %in% names(popularne),]

Nf<-Np
rownames(Nf)<-gsub("dane_gg","",rownames(Nf))
rownames(Nf)<-gsub("lag","",rownames(Nf))
rownames(Nf)<-gsub("\\(|, -6|\\)","",rownames(Nf))
rownames(Nf)<-gsub(", -5","",rownames(Nf))
rownames(Nf)<-gsub(", -4","",rownames(Nf))
rownames(Nf)<-gsub(", -3","",rownames(Nf))
rownames(Nf)<-gsub(", -2","",rownames(Nf))
rownames(Nf)<-gsub(", -1","",rownames(Nf))
rownames(Nf)<-gsub("\\.","",rownames(Nf))

segmenty<-read.csv("./Dane/Car_slownik.csv",sep=";")
segmenty[,2]<-as.character(segmenty[,2])
segmenty[segmenty[,2] %in% c("Akcesoria","Pomoc_spoleczna","Prawne","Sprzedaz"),2]<-"Inne"
table(segmenty[,2])
round(100*table(segmenty[,2])/sum(table(segmenty[,2])),2)
NAZWY_freq<-data.frame(names=rownames(Nf),Nf)
NAZWY_freq<-merge(NAZWY_freq,segmenty,by.x=1,by.y=1)

NAZWY_freq<-aggregate(NAZWY_freq[,-c(1,ncol(NAZWY_freq))],by=list(NAZWY_freq[,"Grupa"]),sum)
rownames(NAZWY_freq)<-NAZWY_freq[,1]
NAZWY_freq<-apply(NAZWY_freq[,-1],2,function(x){return(x/sum(x))})
NAZWY_freq<-ts(t(NAZWY_freq),start=c(2012,6),freq=12)

dev.new()
plot(NAZWY_freq[,1],lwd=2,col=rainbow(8)[1],ylim=c(0,0.6),main="Topic groups share in information sets",ylab="%")
lines(NAZWY_freq[,2],lwd=2,col=rainbow(8)[2])
lines(NAZWY_freq[,3],lwd=2,col=rainbow(8)[3])
lines(NAZWY_freq[,4],lwd=2,col=rainbow(8)[4])
lines(NAZWY_freq[,5],lwd=2,col=rainbow(8)[5])
lines(NAZWY_freq[,6],lwd=2,col=rainbow(8)[6])
lines(NAZWY_freq[,7],lwd=2,col=rainbow(8)[7])
lines(NAZWY_freq[,8],lwd=2,col=rainbow(8)[8])
abline(v=2012+(2/12)+(min(Regimes[[2]]+2)/12),col="blue",lty=4)
abline(v=2012+(2/12)+(min(Regimes[[3]]+2)/12),col="blue",lty=4)
legend("top",legend=colnames(NAZWY_freq),col=rainbow(8),ncol=3,pch=15,bg="white",xpd=TRUE, cex = 1.2)
dev.print(pdf,"LionShare.pdf")


Np<-Np[order(apply(Np,1,sum)),]

cols <- c(
'0' = "#FFFFFF",
'1' = "#FF0000"
)

dev.new()
plot(ts(c(rep(NA,3),persystencja),start=c(2012,6),freq=12),ylab=expression(paste(rho,"(1)",sep="")),main="Persistency in given period",ylim=c(0,1))
abline(h=min(persystencja[-sapply(Regimes,function(x){x[1]-1})]),lwd=2)
abline(h=mean(persystencja[-sapply(Regimes,function(x){x[1]-1})]),lty=4,lwd=2)
min(persystencja[-sapply(Regimes,function(x){x[1]-1})])
mean(persystencja[-sapply(Regimes,function(x){x[1]-1})])
mtext(
	round(min(persystencja[-sapply(Regimes,function(x){x[1]-1})]),2),
	4,
	adj=min(persystencja[-sapply(Regimes,function(x){x[1]-1})])
)
mtext(
	round(mean(persystencja[-sapply(Regimes,function(x){x[1]-1})]),2),
	4,
	adj=mean(persystencja[-sapply(Regimes,function(x){x[1]-1})])
)
dev.print(pdf,"Pers.pdf")

dev.new(width=45,height=45)
image(1:ncol(Np),1:nrow(Np),t(Np),col=cols,main=paste("Google Trends' use"),xlab="Period",ylab="")

order(-apply(
	apply(
		rbind(
			apply(blad_1M_new[,43:51],1,rmse),
			apply(rbind(blad_3M_new[,43:51],matrix(rep(NA,2*9),ncol=9,nrow=2)),1,rmse),
			apply(rbind(blad_6M_new[,43:51],matrix(rep(NA,5*9),ncol=9,nrow=5)),1,rmse),
			apply(rbind(blad_12M_new[,43:51],matrix(rep(NA,11*9),ncol=9,nrow=11)),1,rmse)
		),1,function(x){return(x/mean(x[!is.na(x)]))})
	,1,function(x){return(min(x[!is.na(x)]))}))+2


BESTY

for(i in 1:length(Regimes)){
	temp<-get(paste("final_results_R",i,sep=""))
	rownames(temp)<-rownames(final_results)
	colnames(temp)<-colnames(final_results)
	print(paste("Regime #",i))
	print(temp)

	temp<-get(paste("final_results_o3R",i,sep=""))
	rownames(temp)<-rownames(final_results)
	colnames(temp)<-colnames(final_results)
	print(paste("Regime -3 #",i))
	print(temp)
}
rownames(final_dm_results_o3)<-rownames(final_results_o3)<-rownames(final_results)
colnames(final_dm_results_o3)<-colnames(final_results_o3)<-colnames(final_results)

final_results_o3
final_dm_results_o3

final_results
final_dm_results