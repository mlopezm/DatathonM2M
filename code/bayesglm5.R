
# source("loaddat.R")
# source("preparedat.R")
load("dat.ini.Rdat")

source("librarycode.R")
library(lubridate)
library(ggplot2)
library(dplyr)
library(caret)
# library(glm2)
library(arm)

set.seed(127)
#Obtener datos
#Extraer los datos en wide format para varias sim y luego hacer plot de su actividad
h2_1=2
if(h2_1==2){
#   sim.ids=sample(valsims, 200, replace=F)
#   sim.uwg=towidef2h(sim.ids)  # transformamos a wide format
  load("sim.uwgh2_4_xtra.all.new.Rdat")  # tiene intervalos de 4 horas y resuelve el problema de imputacion de 2h al principio
} else {
#   sim.ids=sample(valsims, 200, replace=F)
#   sim.uwg=towidef1h(sim.ids)  # transformamos a wide format
  load("sim.uwgh1.all.Rdat")
}
  
#Adecuar datos
sim.uwg[sim.uwg$hit==T,"hit"]=1
sim.uwg[sim.uwg$hit==F,"hit"]=0
sim.uwg$wd=as.factor(sim.uwg$wd)
sim.uwg$hd=as.factor(sim.uwg$hd)
sim.uwg$h2d=as.factor(sim.uwg$h2d)
sim.uwg$day=as.factor(sim.uwg$day)
sim.uwg$h4d=as.factor(sim.uwg$h4d)



################################################################################################
# ## Cross Validation
# set.seed(123)
# k=10
# historyruns=NULL
# historyrunstotal=NULL
# count=0
# 
# 
# for (sid in sim.ids){
#   tt= sim.uwg %>% filter(iccid==sid)
#   vallossf=NULL
#   count=count+1
#   print(count)
#   for (j in seq(3)){
#     fds=createFolds(tt$hit,k=k,list=T,returnTrain = F)
#     for (i in seq(k)){
#       train_tr=tt[-fds[[i]],]
#       train_te=tt[fds[[i]],]
#       
#       if(h2_1==2){
#         lmod=glm(hit ~ wd+h2d,data= train_tr,family=binomial())
#       } else {
#         lmod=glm(hit ~ wd+hd,data= train_tr,family=binomial())
#       }
# 
# #       lmod=glm2(hit ~ wd+h2d+day,data= train_tr,family=binomial())
# #       lmod=bayesglm(hit ~ wd+h2d+day,data= train_tr,family="binomial")  #IMPORTANTE si usamos un intervalo 2horas hay que usar h2d
#       pred=predict(lmod,train_te,type="response")
#       
# #       predo=rep(0,length(pred))
# #       predo[pred>0.5]=1
#       pred[pred>=0.5]=1
#       pred[pred<0.5]=0
#           
#       vallossf=c(vallossf,loss12(pred,train_te$hit)/length(pred) )  #loss function
#     }
#   }
#   mvallossf=mean(vallossf)
#   sdvallossf=sd(vallossf)
# #   quant80=quantile(vallossf,0.8)
# #   quant90=quantile(vallossf,0.9)
# #   logmvallossf=mean(log(vallossf))
# #   logsdvallossf=sd(log(vallossf))
#   historyruns=rbind(historyruns,c( simid=sid,
#                                    
#                                    mean=mvallossf,
#                                    sd=sdvallossf,
# #                                    mmassd=mvallossf+sdvallossf,
# #                                    mmenossd=mvallossf-sdvallossf,
# #                                    
# #                                    logmean=logmvallossf,
# #                                    logsd=logsdvallossf,
# #                                    logmmassd=logmvallossf+logsdvallossf,
# #                                    logmmenossd=logmvallossf-logsdvallossf,
# #                                    
# #                                    quant80=quant80,
# #                                    quant90=quant90,
#                                    h2_1=h2_1
#                                 )
#                       )
# 
# }
# 
# historyrunstotal= apply(historyruns[,c(2,3)],2,mean)




# ###############################################################################################
# ## Usando test-train sets como time series
# #predecir usando como test set un porcentaje del tramo final
# porctg=0.07  #porcentaje del final usado para test
# set.seed(123)
# historyruns=NULL
# historyrunstotal=NULL
# count=0
# 
# 
# for (sid in sim.ids){
#   tt= sim.uwg %>% filter(iccid==sid)
#   vallossf=NULL
#   count=count+1
#   print(count)
#   
#   tr.int= 1:floor((1-porctg)*nrow(tt))
#   train_tr=tt[tr.int,]
#   train_te=tt[-tr.int,]
#   
#   if(h2_1==2){
#     lmod=bayesglm(hit ~ wd+h2d+h4d,data= train_tr,family="binomial")
#   } else {
#     lmod=bayesglm(hit ~ wd+hd+h4d,data= train_tr,family="binomial")
#   }
#   
#   #       lmod=glm2(hit ~ wd+h2d+day,data= train_tr,family=binomial())
#   #       lmod=bayesglm(hit ~ wd+h2d+day,data= train_tr,family="binomial")  #IMPORTANTE si usamos un intervalo 2horas hay que usar h2d
#   pred=predict(lmod,train_te,type="response")
#   
#   pred[pred>=0.5]=1
#   pred[pred<0.5]=0
#   
#   vallossf=loss12(pred,train_te$hit)/length(pred)   #loss function
# 
#   historyruns=rbind(historyruns,c( simid=sid,
#                                    
#                                    loss=vallossf,
#                                    h2_1=h2_1
#   )
#   )
#   
# }
# 
# historyrunsm= mean(historyruns[,2])
# historyrunssd= sd(historyruns[,2])
# historyrunsm
# historyrunssd
# save (historyruns,file="hr_bayesglm4_all_2h_4hd_ts.0.7tail.Rdat")
# 
# hr=as.data.frame(historyruns)
# ggplot(hr, aes(x=loss)) + geom_density()
# quantile(hr$loss, seq(0,1,by=0.1))
# table(hr$loss)
# 
# sample_good=hr$simid[hr$loss==1]
# sample_bad=hr$simid[hr$loss==-1]
# save(sample_good,sample_bad,file="./sample_gb/samp_badgood_bayesglm42.Rdat")




# ################################################################################################
# ## Usando test-train sets como time series
# #predecir usando como test set las fechas que vamos a valorar (test valores de reto)
# set.seed(123)
# historyruns=NULL
# historyrunstotal=NULL
# count=0
# 
# #Select account with an iccid
# accounts= unique(sim.uwg$acc)
# 
# #TODO
# 
# 
# true.pred.ts.reto=NULL # true and forecast predictions for ts reto valiadation
# fore.pred.ts.reto=NULL
# for (sid in sim.ids){
#   tt= sim.uwg %>% filter(iccid==sid)
#   vallossf=NULL
#   count=count+1
#   print(count)
#   
#   
#   if(h2_1==2){
#     tr.int= 1:which(tt$d=="2015-04-24 22:00:00")
#     train_tr=tt[tr.int,]
#     train_te=tt[c(262,277),]
#     lmod=bayesglm(hit ~ wd+h2d+h4d,data= train_tr,family="binomial")
#   } else {
#     tr.int= 1:which(tt$d=="2015-04-24 23:00:00")
#     train_tr=tt[tr.int,]
#     train_te=tt[c(523,524,553,554),]
#     lmod=bayesglm(hit ~ wd+hd+h4d,data= train_tr,family="binomial")
#   }
#   
#   #       lmod=glm2(hit ~ wd+h2d+day,data= train_tr,family=binomial())
#   #       lmod=bayesglm(hit ~ wd+h2d+day,data= train_tr,family="binomial")  #IMPORTANTE si usamos un intervalo 2horas hay que usar h2d
#   pred=predict(lmod,train_te,type="response")
#   
#   pred[pred>=0.5]=1
#   pred[pred<0.5]=0
#   
#   vallossf=loss12(pred,train_te$hit)/length(pred)   #loss function
#   
#   historyruns=rbind(historyruns,c( simid=sid,
#                                    
#                                    loss=vallossf,
#                                    h2_1=h2_1
#   )
#   )
#   true.pred.ts.reto = rbind(true.pred.ts.reto,train_te$hit)
#   fore.pred.ts.reto = rbind(fore.pred.ts.reto,pred)
# }
# colnames(true.pred.ts.reto) = c("ta","tb")
# colnames(fore.pred.ts.reto) = c("fa","fb")
# pred.ts.reto=cbind(true.pred.ts.reto,fore.pred.ts.reto)
# save(pred.ts.reto,file="bayesglm4.pred.ts.reto.Rdat")
# 
# 
# historyrunsm= mean(historyruns[,2])
# historyrunssd= sd(historyruns[,2])
# historyrunsm
# historyrunssd
# save (historyruns,file="hr_bayesglm4_all_2h_4hd_ts.reto.Rdat")
# 
# 
# hr=as.data.frame(historyruns)
# ggplot(hr, aes(x=loss)) + geom_density()
# quantile(hr$loss, seq(0,1,by=0.1))
# table(hr$loss)
# 
# sample_good=hr$simid[hr$loss==1]
# sample_bad=hr$simid[hr$loss==-1]
# save(sample_good,sample_bad,file="./sample_gb/samp_badgood_bayesglm43.Rdat")





################################################################################################
## RESULTADO FINAL
#predecir usando datos totales

h2_1=2
if(h2_1==2){
  #   sim.ids=sample(valsims, 200, replace=F)
  #   sim.uwg=towidef2h(sim.ids)  # transformamos a wide format
  load("sim.uwgh2_4_xtra.all.new.Rdat")  # tiene intervalos de 4 horas y resuelve el problema de imputacion de 2h al principio
} else {
  #   sim.ids=sample(valsims, 200, replace=F)
  #   sim.uwg=towidef1h(sim.ids)  # transformamos a wide format
  load("sim.uwgh1.all.Rdat")
}

#Adecuar datos
load("train_te_final.Rdat")
salida=train_te_t[,"sal"]

#junatmos train y test para poder factorizar con los mismos niveles
sim.total=rbind(sim.uwg,train_te_t[,-ncol(train_te_t)])
sim.total[sim.total$hit==T,"hit"]=1
sim.total[sim.total$hit==F,"hit"]=0
sim.total$wd=as.factor(sim.total$wd)
sim.total$hd=as.factor(sim.total$hd)
sim.total$h2d=as.factor(sim.total$h2d)
sim.total$day=as.factor(sim.total$day)
sim.total$h4d=as.factor(sim.total$h4d)

#Sacar datos de train y test finales, los separamos
sim.uwg=sim.total[1:2038192,]
train_te_t=sim.total[-c(1:2038192),]
train_te_t$sal=salida



set.seed(123)
count=0

pred_m=data.frame(iccid=c(0,0),sal=c("A","B"),hit=c(0,0))
pred.final=NULL
for (sid in sim.ids){
  tt= sim.uwg %>% filter(iccid==sid)
  train_tr=tt
  train_te=train_te_t %>% filter(iccid==sid)
  count=count+1
  print(count)
  
  
  if(h2_1==2){
    lmod=bayesglm(hit ~ wd+h2d,data= train_tr,family="binomial")
  } else {
    #lmod=bayesglm(hit ~ wd+hd,data= train_tr,family="binomial")
  }
  
  #       lmod=glm2(hit ~ wd+h2d+day,data= train_tr,family=binomial())
  #       lmod=bayesglm(hit ~ wd+h2d+day,data= train_tr,family="binomial")  #IMPORTANTE si usamos un intervalo 2horas hay que usar h2d
  pred=predict(lmod,train_te,type="response")
  
  pred[pred>=0.5]=1
  pred[pred<0.5]=0
  
  
  pred_m[,1]=sid
  pred_m[,3]=pred
  pred.final = rbind(pred.final,pred_m)
}
colnames(pred.final) = c("iccid","sal","hit")
save(pred.final,file="FINAL.bayesglm5.pred.Rdat")



