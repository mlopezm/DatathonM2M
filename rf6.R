
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
library(randomForest)


set.seed(12314)
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
sim.uwg$hit=as.factor(sim.uwg$hit)  # hit como factor, en funcion de esto cambiar algunos parametros del predict de randomforest


################################################################################################
# ## Cross Validation
# set.seed(12134)
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
# #       mtry=4
# #       sampsize=50
# #       nodesize=2
# #       ntree=501
# #       replace=F
# #       mrf = randomForest(hit ~ wd+h2d, data=train_tr,  
# #                          mtry=mtry,
# #                          sampsize=sampsize, 
# #                          nodesize=nodesize, 
# #                          ntree=ntree,
# #                          replace=replace,
# #                          importance=T) # odd number of trees https://www.biostars.org/p/86981/  # probar con 501, 1001 y 2001
# #       pred =predict(mrf, train_te, type = "response")
# #       impso=importance(mrf)
#       
#       if(h2_1==2){
#         mrf= randomForest(hit ~ wd+h2d, data=train_tr,importance=F)
#       } else {
#         mrf= randomForest(hit ~ wd+hd, data=train_tr,importance=F)
#       }
# 
#       pred=predict(mrf,train_te,type="response")
# #     pred=predict(mrf,train_te,type="prob") # si es factor hit
# #       impso=importance(mrf)
#       
# #       pred=pred[,2] # si es factor hit
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
# historyrunstotal= apply(historyruns[,c(2,3)],2,mean)



###############################################################################################
## Usando test-train sets como time series
#predecir usando como test set un porcentaje del tramo final
porctg=0.07  #porcentaje del final usado para test
set.seed(12134)
historyruns=NULL
historyrunstotal=NULL
count=0


for (sid in sim.ids){
  tt= sim.uwg %>% filter(iccid==sid)
  vallossf=NULL
  count=count+1
  print(count)
  
  tr.int= 1:floor((1-porctg)*nrow(tt))
  train_tr=tt[tr.int,]
  train_te=tt[-tr.int,]
  
  if (sum(train_tr$hit!="0")==0 | sum(train_tr$hit!="1")==0){ # si todos son 0 o 1
    pred=rep(as.numeric(as.character(train_tr$hit[1])),nrow(train_te))  # la prediccion de test es el mismo valor que train
  }else{
  
    if(h2_1==2){
      mrf= randomForest(hit ~ wd+h2d+h4d, data=train_tr,importance=F)
    } else {
      mrf= randomForest(hit ~ wd+hd+h4d, data=train_tr,importance=F)
    }
    
    #   pred=predict(mrf,train_te,type="response")
    pred=predict(mrf,train_te,type="prob")  # si es factor hit
    #   impso=importance(mrf)
    
    pred=pred[,2]  # si es factor hit
    pred[pred>=0.5]=1
    pred[pred<0.5]=0
  }
  vallossf=loss12(pred,train_te$hit)/length(pred)   #loss function
  
  historyruns=rbind(historyruns,c( simid=sid,
                                   
                                   loss=vallossf,
                                   h2_1=h2_1
  )
  )
  
}

historyrunsm= mean(historyruns[,2])
historyrunssd= sd(historyruns[,2])
historyrunsm
historyrunssd
save (historyruns,file="hr_rf6_all_2h_4hd_ts0.7tail.Rdat")

hr=as.data.frame(historyruns)
ggplot(hr, aes(x=loss)) + geom_density()
quantile(hr$loss, seq(0,1,by=0.1))
table(hr$loss)

sample_good=hr$simid[hr$loss==1]
sample_bad=hr$simid[hr$loss==-1]
save(sample_good,sample_bad,file="./sample_gb/samp_badgood_rf62.Rdat")






# ################################################################################################
# ## Usando test-train sets como time series
# #predecir usando como test set las fechas que vamos a valorar (test valores de reto)
# set.seed(12314)
# historyruns=NULL
# historyrunstotal=NULL
# count=0
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
#   } else {
#     tr.int= 1:which(tt$d=="2015-04-24 23:00:00")
#     train_tr=tt[tr.int,]
#     train_te=tt[c(523,524,553,554),]
#   }
#   
#   if (sum(train_tr$hit!="0")==0 | sum(train_tr$hit!="1")==0){ # si todos son 0 o 1
#     pred=rep(as.numeric(as.character(train_tr$hit[1])),nrow(train_te))  # la prediccion de test es el mismo valor que train
#   }else{
#     
#     if(h2_1==2){
#       mrf= randomForest(hit ~ wd+h2d+h4d, data=train_tr,importance=F)
#     } else {
#       mrf= randomForest(hit ~ wd+hd+h4d, data=train_tr,importance=F)
#     }
#     
#     #   pred=predict(mrf,train_te,type="response")
#     pred=predict(mrf,train_te,type="prob")  # si es factor hit
#     #   impso=importance(mrf)
#     
#     pred=pred[,2]  # si es factor hit
#     pred[pred>=0.5]=1
#     pred[pred<0.5]=0
#   }
#   vallossf=loss12(pred,train_te$hit)/length(pred)   #loss function
#   
#   historyruns=rbind(historyruns,c( simid=sid,
#                                    
#                                    loss=vallossf,
#                                    h2_1=h2_1
#   )
#   )
#   true.pred.ts.reto = rbind(true.pred.ts.reto,as.numeric(as.character(train_te$hit)))  # si es factor hit
# #   true.pred.ts.reto = rbind(true.pred.ts.reto,train_te$hit)  
#   fore.pred.ts.reto = rbind(fore.pred.ts.reto,pred)
# }
# colnames(true.pred.ts.reto) = c("ta","tb")
# colnames(fore.pred.ts.reto) = c("fa","fb")
# pred.ts.reto=cbind(true.pred.ts.reto,fore.pred.ts.reto)
# save(pred.ts.reto,file="rf6.pred.ts.reto.Rdat")
# 
# 
# historyrunsm= mean(historyruns[,2])
# historyrunssd= sd(historyruns[,2])
# historyrunsm
# historyrunssd
# save (historyruns,file="hr_rf6_all_2h_4hd_ts.reto.Rdat")
# 
# 
# hr=as.data.frame(historyruns)
# ggplot(hr, aes(x=loss)) + geom_density()
# quantile(hr$loss, seq(0,1,by=0.1))
# table(hr$loss)
# 
# sample_good=hr$simid[hr$loss==1]
# sample_bad=hr$simid[hr$loss==-1]
# save(sample_good,sample_bad,file="./sample_gb/samp_badgood_rf63.Rdat")
# 
# 
# 
# 
# 
# ################################################################################################
# ## RESULTADO FINAL
# #predecir usando datos totales
# 
# h2_1=2
# if(h2_1==2){
#   #   sim.ids=sample(valsims, 200, replace=F)
#   #   sim.uwg=towidef2h(sim.ids)  # transformamos a wide format
#   load("sim.uwgh2_4_xtra.all.new.Rdat")  # tiene intervalos de 4 horas y resuelve el problema de imputacion de 2h al principio
# } else {
#   #   sim.ids=sample(valsims, 200, replace=F)
#   #   sim.uwg=towidef1h(sim.ids)  # transformamos a wide format
#   load("sim.uwgh1.all.Rdat")
# }
# 
# #Adecuar datos
# load("train_te_final.Rdat")
# salida=train_te_t[,"sal"]
# sim.total=rbind(sim.uwg,train_te_t[,-ncol(train_te_t)])
# 
# 
# sim.total[sim.total$hit==T,"hit"]=1
# sim.total[sim.total$hit==F,"hit"]=0
# sim.total$wd=as.factor(sim.total$wd)
# sim.total$hd=as.factor(sim.total$hd)
# sim.total$h2d=as.factor(sim.total$h2d)
# sim.total$day=as.factor(sim.total$day)
# sim.total$hit=as.factor(sim.total$hit)  # hit como factor, en funcion de esto cambiar algunos parametros del predict de randomforest
# 
# #Sacar datos de train y test finales
# sim.uwg=sim.total[1:2038192,]
# train_te_t=sim.total[-c(1:2038192),]
# train_te_t$sal=salida
# 
# 
# 
# set.seed(1234)
# count=0
# 
# 
# pred_m=data.frame(iccid=c(0,0),sal=c("A","B"),hit=c(0,0))
# pred.final=NULL
# for (sid in sim.ids){
#   tt= sim.uwg %>% filter(iccid==sid)
#   train_tr=tt
#   train_te=train_te_t %>% filter(iccid==sid)
#   count=count+1
#   print(count)
#   
#   if (sum(train_tr$hit!="0")==0 | sum(train_tr$hit!="1")==0){ # si todos son 0 o 1
#     pred=rep(as.numeric(as.character(train_tr$hit[1])),nrow(train_te))  # la prediccion de test es el mismo valor que train
#   }else{
#     if(h2_1==2){
#       mrf= randomForest(hit ~ wd+h2d+h4d, data=train_tr,importance=F)
#     } else {
#       #TODO
#     }
#     
#     #   pred=predict(mrf,train_te,type="response")
#     pred=predict(mrf,train_te,type="prob")  # si es factor hit
#     
#     pred=pred[,2]  # si es factor hit
#     pred[pred>=0.5]=1
#     pred[pred<0.5]=0
#   }
#   
#   pred_m[,1]=sid
#   pred_m[,3]=pred
#   pred.final = rbind(pred.final,pred_m)
# }
# colnames(pred.final) = c("iccid","sal","hit")
# save(pred.final,file="FINAL.rf6.pred.Rdat")
