
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
library(xts)
library(forecast)
library(quantmod)
library(corrplot)


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
# sim.uwg$wd=as.factor(sim.uwg$wd)
# sim.uwg$hd=as.factor(sim.uwg$hd)
# sim.uwg$h2d=as.factor(sim.uwg$h2d)
# sim.uwg$day=as.factor(sim.uwg$day)
# sim.uwg$h4d=as.factor(sim.uwg$h4d)


################################################################################################
# ## Usando test-train sets como time series
# #predecir usando como test set un porcentaje del tramo final
porctg=0.07  #porcentaje del final usado para test
set.seed(123)
historyruns=NULL
historyrunstotal=NULL
count=0

true.pred.ts.reto=NULL # true and forecast predictions for ts reto valiadation
fore.pred.ts.reto=NULL
for (sid in sim.ids){
  tt= sim.uwg %>% filter(iccid==sid)
  vallossf=NULL
  count=count+1
  print(count)
  
  #   xall <- xts(tt$vol,as.POSIXlt(tt$d, format="%Y-%m-%d %H:%M:%S"))
  #   chartSeries(xall)
  
  tr.int= 1:floor((1-porctg)*nrow(tt))
  train_tr=tt[tr.int,]
  train_te=tt[-tr.int,]
  
  if (sum(train_tr$hit!="0")<4 | sum(train_tr$hit!="1")<4){ # si casi todos son 0 o 1
    pred=rep(round(mean(train_tr$hit)),nrow(train_te))  # la prediccion de test es el mismo valor que train
  }else{
    if(h2_1==2){
      ft=as.POSIXlt(train_tr$d, format="%Y-%m-%d %H:%M:%S")
      xtr <- xts(train_tr$hit,ft)
      #     chartSeries(xtr)
      fit=ets(xtr)
      #     fit
      maxlag=nrow(train_te)
      pred=forecast(fit,h=maxlag)
      #     plot(pred)
      pred=pred$mean[1:maxlag]
    } else {
      xtr <- xts(train_tr$vol,as.POSIXlt(train_tr$d, format="%Y-%m-%d %H:%M:%S"))
      fit=ets(xtr)
      
      # TODO
    }
  }
  #Transforma la prediccion de volumen en hit
  pred[pred>0.5]=1
  pred[pred<=0.5]=0
  
  vallossf=loss12(pred,train_te$hit)/length(pred)   #loss function
  
  historyruns=rbind(historyruns,c( simid=sid,
                                   
                                   loss=vallossf,
                                   h2_1=h2_1
  )
  )
  true.pred.ts.reto = rbind(true.pred.ts.reto,train_te$hit)
  fore.pred.ts.reto = rbind(fore.pred.ts.reto,pred)
}
colnames(true.pred.ts.reto) = paste0("t",1:ncol(true.pred.ts.reto))
colnames(fore.pred.ts.reto) = paste0("f",1:ncol(fore.pred.ts.reto))
pred.ts.reto=cbind(true.pred.ts.reto,fore.pred.ts.reto)
save(pred.ts.reto,file="timeseries_ets2.pred.ts.0.7tail.Rdat")


historyrunsm= mean(historyruns[,2])
historyrunssd= sd(historyruns[,2])
historyrunsm
historyrunssd
save (historyruns,file="hr_timeserie_ets2_all_2h_4hd_ts.0.7tail.Rdat")


hr=as.data.frame(historyruns)
ggplot(hr, aes(x=loss)) + geom_density()
quantile(hr$loss, seq(0,1,by=0.1))
table(hr$loss)

sample_good=hr$simid[hr$loss==1]
sample_bad=hr$simid[hr$loss==-1]
save(sample_good,sample_bad,file="./sample_gb/samp_badgood_timeseries_ets22.Rdat")









################################################################################################
## Usando test-train sets como time series
#predecir usando como test set las fechas que vamos a valorar (test valores de reto)
set.seed(123)
historyruns=NULL
historyrunstotal=NULL
count=0

true.pred.ts.reto=NULL # true and forecast predictions for ts reto valiadation
fore.pred.ts.reto=NULL
for (sid in sim.ids){
  tt= sim.uwg %>% filter(iccid==sid)
  vallossf=NULL
  count=count+1
  print(count)
  
#   xall <- xts(tt$vol,as.POSIXlt(tt$d, format="%Y-%m-%d %H:%M:%S"))
#   chartSeries(xall)
  if(h2_1==2){
    tr.int= 1:which(tt$d=="2015-04-24 22:00:00")
    train_tr=tt[tr.int,]
    train_te=tt[c(262,277),]
  } else {
    tr.int= 1:which(tt$d=="2015-04-24 23:00:00")
    train_tr=tt[tr.int,]
    train_te=tt[c(523,524,553,554),]
  }
  
  if (sum(train_tr$hit!="0")<4 | sum(train_tr$hit!="1")<4){ # si casi todos son 0 o 1
    pred=rep(round(mean(train_tr$hit)),nrow(train_te))  # la prediccion de test es el mismo valor que train
  }else{
    if(h2_1==2){
      ft=as.POSIXlt(train_tr$d, format="%Y-%m-%d %H:%M:%S")
      xtr <- xts(train_tr$hit,ft)
  #     chartSeries(xtr)
      fit=ets(xtr)
  #     fit
      pred=forecast(fit,h=33)
  #     plot(pred)
      pred=pred$mean[c(18,33)]
    } else {
      xtr <- xts(train_tr$vol,as.POSIXlt(train_tr$d, format="%Y-%m-%d %H:%M:%S"))
      fit=ets(xtr)
      
      # TODO
    }
  }
  #Transforma la prediccion de volumen en hit
  pred[pred>0.5]=1
  pred[pred<=0.5]=0
  
  vallossf=loss12(pred,train_te$hit)/length(pred)   #loss function
  
  historyruns=rbind(historyruns,c( simid=sid,
                                   
                                   loss=vallossf,
                                   h2_1=h2_1
  )
  )
  true.pred.ts.reto = rbind(true.pred.ts.reto,train_te$hit)
  fore.pred.ts.reto = rbind(fore.pred.ts.reto,pred)
}
colnames(true.pred.ts.reto) = c("ta","tb")
colnames(fore.pred.ts.reto) = c("fa","fb")
pred.ts.reto=cbind(true.pred.ts.reto,fore.pred.ts.reto)
save(pred.ts.reto,file="timeseries_ets2.pred.ts.reto.Rdat")


historyrunsm= mean(historyruns[,2])
historyrunssd= sd(historyruns[,2])
historyrunsm
historyrunssd
save (historyruns,file="hr_timeserie_ets2_all_2h_4hd_ts.reto.Rdat")


hr=as.data.frame(historyruns)
ggplot(hr, aes(x=loss)) + geom_density()
quantile(hr$loss, seq(0,1,by=0.1))
table(hr$loss)

sample_good=hr$simid[hr$loss==1]
sample_bad=hr$simid[hr$loss==-1]
save(sample_good,sample_bad,file="./sample_gb/samp_badgood_timeseries_ets23.Rdat")





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
sim.total=rbind(sim.uwg,train_te_t[,-ncol(train_te_t)])

sim.total[sim.total$hit==T,"hit"]=1
sim.total[sim.total$hit==F,"hit"]=0
# sim.total$wd=as.factor(sim.total$wd)
# sim.total$hd=as.factor(sim.total$hd)
# sim.total$h2d=as.factor(sim.total$h2d)
# sim.total$day=as.factor(sim.total$day)
# sim.total$h4d=as.factor(sim.total$h4d)

#Sacar datos de train y test finales
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
  
  if (sum(train_tr$hit!="0")<4 | sum(train_tr$hit!="1")<4){ # si casi todos son 0 o 1
    pred=rep(round(mean(train_tr$hit)),nrow(train_te))  # la prediccion de test es el mismo valor que train
  }else{
    if(h2_1==2){
      ft=as.POSIXlt(train_tr$d, format="%Y-%m-%d %H:%M:%S")
      xtr <- xts(train_tr$hit,ft)
      #     chartSeries(xtr)
      fit=ets(xtr)
      #     fit
      pred=forecast(fit,h=33)
      #     plot(pred)
      pred=pred$mean[c(18,33)]
    } else {
      xtr <- xts(train_tr$vol,as.POSIXlt(train_tr$d, format="%Y-%m-%d %H:%M:%S"))
      fit=ets(xtr)
      
      # TODO
    }
  }
  #Transforma la prediccion de volumen en hit
  pred[pred>0.5]=1
  pred[pred<=0.5]=0
  
  
  pred_m[,1]=sid
  pred_m[,3]=pred
  pred.final = rbind(pred.final,pred_m)
}
colnames(pred.final) = c("iccid","sal","hit")
save(pred.final,file="FINAL.ets2.pred.Rdat")


