
library(plotmo)
library(plotrix)
library(earth)
library(Metrics)
library(MLmetrics)


setwd("C:/Users/barisoztas/Desktop/CE-STAR/danish_lakes/Machine_Learning/mars_folder/landsat8_group2")

res<- read.table("trng_y.csv",header=T,sep=",")
obs<-read.table("trng_x.csv",header=T,sep=",")
y_val<-read.table("y_val.csv",header=T,sep=",")
x_val<-read.table("x_val.csv",header=T,sep=",")


count=5
CPU<-0
GCV<-0
RSQ<-0
RSS<-0

rmse_trng<-0
rmse_val<-0

r_trng<-0
r_val<-0




result<-0



for (i in 1:10){

start_time <- as.numeric(Sys.time(),digits=10)

deneme1<-earth (res,obs,trace = 1, nk=15, degree=3 ,penalty=3, thresh=0.000001)
summary(deneme1, digits = 4)
ev <- evimp(deneme1, trim=FALSE, sqrt.=TRUE)
print(ev)

ev <- evimp(deneme1, trim=FALSE, sqrt.=TRUE)

end_time <- as.numeric(Sys.time(),digits=10)


xx <- end_time - start_time
CPU[i]<-xx
GCV[i]<-deneme1$gcv
RSQ[i]<-deneme1$rsq
RSS[i]<-deneme1$rss


print(xx)

trng<-predict(deneme1, res)
val<-predict(deneme1, y_val)


rmse_trng[i]<-rmse(data.matrix(obs),data.matrix(trng))
rmse_val[i]<-rmse(data.matrix(x_val),data.matrix(val))


r_trng[i]<-cor(obs,trng,method = "pearson")
r_val[i]<-cor(x_val,val,method = "pearson")



BF <- as.character(count) 

write.table(trng,paste("MARS_TRNG_Result_BF_", BF, "_DG_1.csv", sep=""),sep=",",col.names=TRUE,row.names=FALSE)
write.table(val,paste("MARS_VAL_Result_BF_", BF, "_DG_1.csv", sep=""),sep=",",col.names=TRUE,row.names=FALSE)



count=count+5
result<-cbind(rmse_trng,r_trng,rmse_val,r_val)
result



}

result<-cbind(rmse_trng,r_trng,rmse_val,r_val)

write.table(CPU,"01_CPU_DG_1.csv",sep=",",col.names=FALSE,row.names=FALSE)
write.table(GCV,"02_GCV_DG_1.csv",sep=",",col.names=FALSE,row.names=FALSE)
write.table(RSQ,"03_RSQ_DG_1.csv",sep=",",col.names=FALSE,row.names=FALSE)
write.table(RSS,"04_RSS_DG_1.csv",sep=",",col.names=FALSE,row.names=FALSE)


write.table(result,"RESULT.csv",sep=",",col.names=FALSE,row.names=FALSE)






summary(deneme1, digits = 4)

ev <- evimp(deneme1, trim=FALSE, sqrt.=TRUE)
plot(ev)
print(ev)

