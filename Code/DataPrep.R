dane_m<-ts(head(read.csv("./Dane/DaneX.csv",sep=";",dec=".")[,-1],-3),start=c(2007,1),freq=12)

listadat<-daty(pocz,nrow(dane_m))
dni<-listadat$dni
dni_fory<-listadat$fory

rynek_raw<-dane_m[,1]

#set.seed(2017)
#rynek_raw<-cumsum(rnorm(nrow(dane_m),0,1))
rynek<-rynek_raw/dni[,2]

outliers1<-as.matrix(rep(0,length(rynek)))
outliers2<-outliers1
outliers1[c(47:48),1]<-1
outliers2[c(86,87),1]<-1
outliers<-cbind(outliers1,outliers2)
colnames(outliers)<-c("r10","r14")

ar_rynek<-Arima(rynek,order=c(3,0,0),xreg=outliers)

rynek[which(outliers1==1)]<-rynek[which(outliers1==1)]-ar_rynek$coef[5]
rynek[which(outliers2==1)]<-rynek[which(outliers2==1)]-ar_rynek$coef[6]

#rm(outliers1,outliers2,outliers,ar_rynek,rynek_dec)

rynek_dec<-stl(rynek/rynek[1],s.window="periodic")
rynek_xS<-log(rynek_dec$time.series[,2]+rynek_dec$time.series[,3])

wwuk<-dane_m[,2]
wwuk_xS<-hpfilter(wwuk,type="lambda",freq=3)$trend

wynagrodzenie_xS<-log(zrob_zmienna(dane_m[,3])$no.seas)
produkcja_xS<-log(zrob_zmienna(dane_m[,4])$no.seas)
konsumpcja_xS<-log(zrob_zmienna(dane_m[,5])$no.seas)

dane_var<-na.omit(cbind(
		diff(wynagrodzenie_xS),
		diff(rynek_xS),
		diff(wwuk_xS),
		diff(produkcja_xS),
		diff(konsumpcja_xS)
		))

colnames(dane_var)<-c("Wynagrodzenie","Rynek","WWUK","Produkcja","Konsumpcja")
dane_var<-window(dane_var,end=c(2016,12))

sent<-ts(get_sents(dane_var[,-3])$sent,start=c(2007,4),freq=12)
dane_var<-na.omit(cbind(dane_var,sent))

dane_gg<-ts(log(read.csv("./Dane/GDane.csv",sep=";")[,-1]),start=c(2004,1),freq=12)
dane_gg[dane_gg==-Inf]<-0
for (ig in 1:ncol(dane_gg)){
	dane_gg[,ig]<-dane_gg[,ig]-stl(dane_gg[,ig],s.window="periodic")$time.series[,1]
	dane_gg[,ig]<-hpfilter(ts(dane_gg[,ig],freq=12),type="lambda",freq=1)$trend
}

dane_gg<-cbind(dane_gg,lag(dane_gg,-1),lag(dane_gg,-2),lag(dane_gg,-3),lag(dane_gg,-4),lag(dane_gg,-5),lag(dane_gg,-6))
dane_gg<-diff(dane_gg)
dane_gg<-window(dane_gg,c(2007,4),c(2016,12))
#dane_gg<-dane_gg-apply(dane_gg,1,mean)
dane_gg_sent<-dane_gg

model<-spikeslab(sent~dane_gg_sent,n.iter1=2000,n.inter2=1200,ntree=700,intercept=TRUE,bigp.smalln=TRUE)
substr(rownames(model$summary)[round(model$summary[,1],5)!=0],13,nchar(rownames(model$summary)[round(model$summary[,1],5)!=0]))
pred<-as.matrix(dane_gg_sent)%*%as.matrix(model$gnet.scale)
pred<-ts((pred/sd(pred))+mean(diff(sent)/sd(diff(sent)))-mean(pred/sd(pred)),start=c(2008,5),freq=12)
pred<-c(0,pred)
dane_var<-cbind(dane_var,diff(pred))
