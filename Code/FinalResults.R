for(i in 1:length(Regimes)){
	temp_results<-rbind(
		round(apply(best_bledy_naive[Regimes[[i]],],2,rmse)/apply(best_bledy_arma[Regimes[[i]],],2,rmse)*100,2),
		round(apply(best_bledy_rw[Regimes[[i]],],2,rmse)/apply(best_bledy_arma[Regimes[[i]],],2,rmse)*100,2),
		round(apply(best_bledy_armaxs[Regimes[[i]],],2,rmse)/apply(best_bledy_arma[Regimes[[i]],],2,rmse)*100,2),
		round(apply(best_bledy_armaxw[Regimes[[i]],],2,rmse)/apply(best_bledy_arma[Regimes[[i]],],2,rmse)*100,2),
		round(apply(best_bledy_lstar[Regimes[[i]],],2,rmse)/apply(best_bledy_arma[Regimes[[i]],],2,rmse)*100,2),
		round(apply(best_bledy_lstar_wwuk[Regimes[[i]],],2,rmse)/apply(best_bledy_arma[Regimes[[i]],],2,rmse)*100,2),
		round(apply(best_bledy_armaxgt[Regimes[[i]],],2,rmse)/apply(best_bledy_arma[Regimes[[i]],],2,rmse)*100,2),
		round(apply(best_bledy_arrnet[Regimes[[i]],],2,rmse)/apply(best_bledy_arma[Regimes[[i]],],2,rmse)*100,2),
		round(apply(best_bledy_gnet[Regimes[[i]],],2,rmse)/apply(best_bledy_arma[Regimes[[i]],],2,rmse)*100,2),
		round(apply(best_bledy_gnet_mean[Regimes[[i]],],2,rmse)/apply(best_bledy_arma[Regimes[[i]],],2,rmse)*100,2)
	)
	assign(paste("final_results_R",i,sep=""),temp_results)
	rm(temp_results)

	temp_results<-rbind(
		round(apply(best_bledy_naive[tail(Regimes[[i]],-3),],2,rmse)/apply(best_bledy_arma[tail(Regimes[[i]],-3),],2,rmse)*100,2),
		round(apply(best_bledy_rw[tail(Regimes[[i]],-3),],2,rmse)/apply(best_bledy_arma[tail(Regimes[[i]],-3),],2,rmse)*100,2),
		round(apply(best_bledy_armaxs[tail(Regimes[[i]],-3),],2,rmse)/apply(best_bledy_arma[tail(Regimes[[i]],-3),],2,rmse)*100,2),
		round(apply(best_bledy_armaxw[tail(Regimes[[i]],-3),],2,rmse)/apply(best_bledy_arma[tail(Regimes[[i]],-3),],2,rmse)*100,2),
		round(apply(best_bledy_lstar[tail(Regimes[[i]],-3),],2,rmse)/apply(best_bledy_arma[tail(Regimes[[i]],-3),],2,rmse)*100,2),
		round(apply(best_bledy_lstar_wwuk[tail(Regimes[[i]],-3),],2,rmse)/apply(best_bledy_arma[tail(Regimes[[i]],-3),],2,rmse)*100,2),
		round(apply(best_bledy_armaxgt[tail(Regimes[[i]],-3),],2,rmse)/apply(best_bledy_arma[tail(Regimes[[i]],-3),],2,rmse)*100,2),
		round(apply(best_bledy_arrnet[tail(Regimes[[i]],-3),],2,rmse)/apply(best_bledy_arma[tail(Regimes[[i]],-3),],2,rmse)*100,2),
		round(apply(best_bledy_gnet[tail(Regimes[[i]],-3),],2,rmse)/apply(best_bledy_arma[Regimes[[i]],],2,rmse)*100,2),
		round(apply(best_bledy_gnet_mean[tail(Regimes[[i]],-3),],2,rmse)/apply(best_bledy_arma[Regimes[[i]],],2,rmse)*100,2)
	)
	assign(paste("final_results_o3R",i,sep=""),temp_results)
	rm(temp_results)
}

#### TOTAL ###
final_results<-rbind(
	round(apply(best_bledy_naive,2,rmse)/apply(best_bledy_arma,2,rmse)*100,2),
	round(apply(best_bledy_rw,2,rmse)/apply(best_bledy_arma,2,rmse)*100,2),
	round(apply(best_bledy_armaxs,2,rmse)/apply(best_bledy_arma,2,rmse)*100,2),
	round(apply(best_bledy_armaxw,2,rmse)/apply(best_bledy_arma,2,rmse)*100,2),
	round(apply(best_bledy_lstar,2,rmse)/apply(best_bledy_arma,2,rmse)*100,2),
	round(apply(best_bledy_lstar_wwuk,2,rmse)/apply(best_bledy_arma,2,rmse)*100,2),
	round(apply(best_bledy_armaxgt,2,rmse)/apply(best_bledy_arma,2,rmse)*100,2),
	round(apply(best_bledy_arrnet,2,rmse)/apply(best_bledy_arma,2,rmse)*100,2),
	round(apply(best_bledy_gnet,2,rmse)/apply(best_bledy_arma,2,rmse)*100,2),
	round(apply(best_bledy_gnet_mean,2,rmse)/apply(best_bledy_arma,2,rmse)*100,2)
)

omin<-c(sapply(Regimes,function(x){return(head(x,3))}))
final_results_o3<-rbind(
	round(apply(best_bledy_naive[-omin,],2,rmse)/apply(best_bledy_arma[-omin,],2,rmse)*100,2),
	round(apply(best_bledy_rw[-omin,],2,rmse)/apply(best_bledy_arma[-omin,],2,rmse)*100,2),
	round(apply(best_bledy_armaxs[-omin,],2,rmse)/apply(best_bledy_arma[-omin,],2,rmse)*100,2),
	round(apply(best_bledy_armaxw[-omin,],2,rmse)/apply(best_bledy_arma[-omin,],2,rmse)*100,2),
	round(apply(best_bledy_lstar[-omin,],2,rmse)/apply(best_bledy_arma[-omin,],2,rmse)*100,2),
	round(apply(best_bledy_lstar_wwuk[-omin,],2,rmse)/apply(best_bledy_arma,2,rmse)*100,2),
	round(apply(best_bledy_armaxgt[-omin,],2,rmse)/apply(best_bledy_arma[-omin,],2,rmse)*100,2),
	round(apply(best_bledy_arrnet[-omin,],2,rmse)/apply(best_bledy_arma[-omin,],2,rmse)*100,2),
	round(apply(best_bledy_gnet[-omin,],2,rmse)/apply(best_bledy_arma[-omin,],2,rmse)*100,2),
	round(apply(best_bledy_gnet_mean[-omin,],2,rmse)/apply(best_bledy_arma[-omin,],2,rmse)*100,2)
)

### FINAL DIEBOLD-MARIANO

final_dm_results<-rbind(
	round(my_dm.test(best_bledy_naive,best_bledy_arma,c(1,3,6,12)),3),
	round(my_dm.test(best_bledy_rw,best_bledy_arma,c(1,3,6,12)),3),
	round(my_dm.test(best_bledy_armaxs,best_bledy_arma,c(1,3,6,12)),3),
	round(my_dm.test(best_bledy_armaxw,best_bledy_arma,c(1,3,6,12)),3),
	round(my_dm.test(best_bledy_lstar,best_bledy_arma,c(1,3,6,12)),3),
	round(my_dm.test(best_bledy_lstar_wwuk,best_bledy_arma,c(1,3,6,12)),3),
	round(my_dm.test(best_bledy_armaxgt,best_bledy_arma,c(1,3,6,12)),3),
	round(my_dm.test(best_bledy_arrnet,best_bledy_arma,c(1,3,6,12)),3),
	round(my_dm.test(best_bledy_gnet,best_bledy_arma,c(1,3,6,12)),3),
	round(my_dm.test(best_bledy_gnet_mean,best_bledy_arma,c(1,3,6,12)),3)
)

final_dm_results_o3<-rbind(
	round(my_dm.test(best_bledy_naive[-omin,],best_bledy_arma[-omin,],c(1,3,6,12)),3),
	round(my_dm.test(best_bledy_rw[-omin,],best_bledy_arma[-omin,],c(1,3,6,12)),3),
	round(my_dm.test(best_bledy_armaxs[-omin,],best_bledy_arma[-omin,],c(1,3,6,12)),3),
	round(my_dm.test(best_bledy_armaxw[-omin,],best_bledy_arma[-omin,],c(1,3,6,12)),3),
	round(my_dm.test(best_bledy_lstar[-omin,],best_bledy_arma[-omin,],c(1,3,6,12)),3),
	round(my_dm.test(best_bledy_lstar[-omin,],best_bledy_arma[-omin,],c(1,3,6,12)),3),
	round(my_dm.test(best_bledy_armaxgt[-omin,],best_bledy_arma[-omin,],c(1,3,6,12)),3),
	round(my_dm.test(best_bledy_arrnet[-omin,],best_bledy_arma[-omin,],c(1,3,6,12)),3),
	round(my_dm.test(best_bledy_gnet[-omin,],best_bledy_arma[-omin,],c(1,3,6,12)),3),
	round(my_dm.test(best_bledy_gnet_mean[-omin,],best_bledy_arma[-omin,],c(1,3,6,12)),3)
)


colnames(final_dm_results)<-colnames(final_results)<-c("1M","3M","6M","12M")
rownames(final_dm_results)<-rownames(final_results)<-c("Naive","RW","ARMAXS","ARMAXW","LSTAR SENT","LSTAR WWUK","ARMAXGT","ARMA-ANN","GNNET","GNNET MEAN")

