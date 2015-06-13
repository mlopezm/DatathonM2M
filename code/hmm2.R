

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
library(depmixS4)
library(xts)
library(forecast)
library(quantmod)
library(corrplot)
# library(trmat)
library(nnet)


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


###############################################################################################
## Usando test-train sets como time series
#predecir usando como test set un porcentaje del tramo final
porctg=0.07  #porcentaje del final usado para test
set.seed(123)
historyruns=NULL
historyrunstotal=NULL
count=0

true.pred.ts.reto=NULL # true and forecast predictions for ts reto valiadation
fore.pred.ts.reto=NULL
outs.from.posterior=NULL  #prediccion de 0 y 1 del HMM por cada muestra de cada iccid
iccid.extremes=NULL  #guardamos los iccid con valores tods 0 1 en casi todos sus valores
for (sid in sim.ids){
  tt= sim.uwg %>% filter(iccid==sid)
  vallossf=NULL
  count=count+1
  print(count)
  
  #   xall <- xts(tt$vol,as.POSIXlt(tt$d, format="%Y-%m-%d %H:%M:%S"))
  #   chartSeries(xall)
  
  states=c(1,2)
  outputs=c(0,1)
  
  tr.int= 1:floor((1-porctg)*nrow(tt))
  train_tr=tt[tr.int,]
  train_te=tt[-tr.int,]
  
  
  if (sum(train_tr$hit!="0")<4 | sum(train_tr$hit!="1")<4){ # si casi todos son 0 o 1
    pred=rep(round(mean(train_tr$hit)),nrow(train_te))  # la prediccion de test es el mismo valor que train
    out.from.posterior=cbind(sid,rep(round(mean(train_tr$hit)),nrow(train_tr)))
    iccid.extremes=c(iccid.extremes,sid)
  }else{
    if(h2_1==2){
      #Vamos a aplicar HMM para detectar operating modes
      # extraemos los parametros del modelo usando Baum Welch y luego la secuencia de estados mas probables usdando viterbi
      # usamos 4 estados hidden y dos valores de salida 1-0
      #usamos el necesitar 10 muestars por cada parametro a calcular, con 4 estados y 2 salidas los parametros son 4*4+2*4=24 y 24*10 es menor que 328
      #set up the model
      set.seed(3)
      mod <- depmix(hit ~ 1, data=train_tr, family=multinomial("identity"),nstates=2)  #elegimos 4 estados ocultos
      #   mod <- depmix(vol ~ 1, data=train_tr, family=gaussian(),nstates=2)  #elegimos 4 estados ocultos
      
      #fit the model 
      f <- fit(mod,verbose=F)
      #     AIC(f);BIC(f)
      #check to see how the state transtion matricies and process mean/sd matches our sample data
      #     summary(f)
      #     f
      
      tranprob=t(f@trDens[1,,])
      emiprob=rbind(f@response[[1]][[1]]@parameters[[1]],
                    f@response[[2]][[1]]@parameters[[1]])
      
      #get the estimated state for each timestep 
      pos <- posterior(f)
      #     ggplot(train_tr, aes(x=hserie,y=vol)) + geom_line()+ geom_segment(aes(x = 1:(nrow(pos)), y = pos$state*1000, xend = 2:(nrow(pos)+1), yend = pos$state*1000),color=as.factor(pos$state))
      
      #   xall <- xts(train_tr$vol,as.POSIXlt(train_tr$d, format="%Y-%m-%d %H:%M:%S"))
      #   chartSeries(xall)
      #   addLines(v=which(pos[,1]==1)) #marcar punto re ruptura 1
      #   y0 = rep(0,nrow(train_tr))
      #   y1=y0; y1[pos$state==1]=1
      #   sfun1  = stepfun(1:(length(y0)-1), y1, f = 0)
      #   plot(sfun1,col=2)
      
      #Prediccion
      maxlag=nrow(train_te)  #longitud de prediccion
      state.now=pos[nrow(pos),1]
      states.pred=NULL
      outs.pred=NULL
      for(i in 1:maxlag){
        state.now=sample(states,1,prob=tranprob[state.now,])
        state.now
        states.pred=c(states.pred,state.now)
        out.now=sample(outputs,1,prob=emiprob[state.now,])
        outs.pred=c(outs.pred,out.now)
      }
      
      pred=outs.pred
      out.from.posterior=cbind(sid,sapply(pos[,1],function(x) sample(outputs,1,prob=emiprob[x,])))
    } else {
      # TODO
    }
  }
  
  vallossf=loss12(pred,train_te$hit)/length(pred)   #loss function
  
  historyruns=rbind(historyruns,c( simid=sid,
                                   
                                   loss=vallossf,
                                   h2_1=h2_1
  )
  )
  
  outs.from.posterior=rbind(outs.from.posterior,out.from.posterior)
  
  
  true.pred.ts.reto = rbind(true.pred.ts.reto,train_te$hit)
  fore.pred.ts.reto = rbind(fore.pred.ts.reto,pred)
  
}
colnames(true.pred.ts.reto) = paste0("t",1:ncol(true.pred.ts.reto))
colnames(fore.pred.ts.reto) = paste0("f",1:ncol(fore.pred.ts.reto))
pred.ts.reto=cbind(true.pred.ts.reto,fore.pred.ts.reto)
save(pred.ts.reto,file="hmm2.pred.ts.0.7tail.Rdat")

colnames(outs.from.posterior)=c("iccid","predhmm")
save(outs.from.posterior,file="hmm2.outsfromposteriors.ts.0.7tail.Rdat")

save(iccid.extremes,file="iccid.extremes.Rdat")



historyrunsm= mean(historyruns[,2])
historyrunssd= sd(historyruns[,2])
historyrunsm
historyrunssd
save (historyruns,file="hr_hmm2_all_2h_4hd_ts.0.7tail.Rdat")


hr=as.data.frame(historyruns)
ggplot(hr, aes(x=loss)) + geom_density()
quantile(hr$loss, seq(0,1,by=0.1))
table(hr$loss)

sample_good=hr$simid[hr$loss==1]
sample_bad=hr$simid[hr$loss==-1]
save(sample_good,sample_bad,file="./sample_gb/samp_badgood_hmm22.Rdat")





###########################################################################################################
## Usando test-train sets como time series
#predecir usando como test set las fechas que vamos a valorar (test valores de reto)
set.seed(123)
historyruns=NULL
historyrunstotal=NULL
count=0

true.pred.ts.reto=NULL # true and forecast predictions for ts reto valiadation
fore.pred.ts.reto=NULL
outs.from.posterior=NULL  #prediccion de 0 y 1 del HMM por cada muestra de cada iccid
iccid.extremes=NULL  #guardamos los iccid con valores tods 0 1 en casi todos sus valores
for (sid in sim.ids){
  tt= sim.uwg %>% filter(iccid==sid)
  vallossf=NULL
  count=count+1
  print(count)
  
#   xall <- xts(tt$vol,as.POSIXlt(tt$d, format="%Y-%m-%d %H:%M:%S"))
#   chartSeries(xall)

  states=c(1,2)
  outputs=c(0,1)

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
    out.from.posterior=cbind(sid,rep(round(mean(train_tr$hit)),nrow(train_tr)))
    iccid.extremes=c(iccid.extremes,sid)
  }else{
    if(h2_1==2){
      #Vamos a aplicar HMM para detectar operating modes
      # extraemos los parametros del modelo usando Baum Welch y luego la secuencia de estados mas probables usdando viterbi
      # usamos 4 estados hidden y dos valores de salida 1-0
      #usamos el necesitar 10 muestars por cada parametro a calcular, con 4 estados y 2 salidas los parametros son 4*4+2*4=24 y 24*10 es menor que 328
      #set up the model
      set.seed(3)
      mod <- depmix(hit ~ 1, data=train_tr, family=multinomial("identity"),nstates=2)  #elegimos 4 estados ocultos
      #   mod <- depmix(vol ~ 1, data=train_tr, family=gaussian(),nstates=2)  #elegimos 4 estados ocultos
      
      #fit the model 
      f <- fit(mod,verbose=F)
  #     AIC(f);BIC(f)
      #check to see how the state transtion matricies and process mean/sd matches our sample data
  #     summary(f)
  #     f
  
      tranprob=t(f@trDens[1,,])
      emiprob=rbind(f@response[[1]][[1]]@parameters[[1]],
                    f@response[[2]][[1]]@parameters[[1]])
  
      #get the estimated state for each timestep 
      pos <- posterior(f)
  #     ggplot(train_tr, aes(x=hserie,y=vol)) + geom_line()+ geom_segment(aes(x = 1:(nrow(pos)), y = pos$state*1000, xend = 2:(nrow(pos)+1), yend = pos$state*1000),color=as.factor(pos$state))
  
      #   xall <- xts(train_tr$vol,as.POSIXlt(train_tr$d, format="%Y-%m-%d %H:%M:%S"))
      #   chartSeries(xall)
      #   addLines(v=which(pos[,1]==1)) #marcar punto re ruptura 1
      #   y0 = rep(0,nrow(train_tr))
      #   y1=y0; y1[pos$state==1]=1
      #   sfun1  = stepfun(1:(length(y0)-1), y1, f = 0)
      #   plot(sfun1,col=2)
      
      #Prediccion
      maxlag=33  #longitud de prediccion
      state.now=pos[nrow(pos),1]
      states.pred=NULL
      outs.pred=NULL
      for(i in 1:maxlag){
        state.now=sample(states,1,prob=tranprob[state.now,])
        state.now
        states.pred=c(states.pred,state.now)
        out.now=sample(outputs,1,prob=emiprob[state.now,])
        outs.pred=c(outs.pred,out.now)
      }
  
      pred=outs.pred[c(18,33)]
      out.from.posterior=cbind(sid,sapply(pos[,1],function(x) sample(outputs,1,prob=emiprob[x,])))
    } else {
        # TODO
    }
  }
  
  vallossf=loss12(pred,train_te$hit)/length(pred)   #loss function
  
  historyruns=rbind(historyruns,c( simid=sid,
                                   
                                   loss=vallossf,
                                   h2_1=h2_1
  )
  )

  outs.from.posterior=rbind(outs.from.posterior,out.from.posterior)
  
  
  true.pred.ts.reto = rbind(true.pred.ts.reto,train_te$hit)
  fore.pred.ts.reto = rbind(fore.pred.ts.reto,pred)

}
colnames(true.pred.ts.reto) = c("ta","tb")
colnames(fore.pred.ts.reto) = c("fa","fb")
pred.ts.reto=cbind(true.pred.ts.reto,fore.pred.ts.reto)
save(pred.ts.reto,file="hmm2.pred.ts.reto.Rdat")

colnames(outs.from.posterior)=c("iccid","predhmm")
save(outs.from.posterior,file="hmm2.outsfromposteriors.ts.reto.Rdat")

save(iccid.extremes,file="iccid.extremes.Rdat")



historyrunsm= mean(historyruns[,2])
historyrunssd= sd(historyruns[,2])
historyrunsm
historyrunssd
save (historyruns,file="hr_hmm2_all_2h_4hd_ts.reto.Rdat")


hr=as.data.frame(historyruns)
ggplot(hr, aes(x=loss)) + geom_density()
quantile(hr$loss, seq(0,1,by=0.1))
table(hr$loss)

sample_good=hr$simid[hr$loss==1]
sample_bad=hr$simid[hr$loss==-1]
save(sample_good,sample_bad,file="./sample_gb/samp_badgood_hmm23.Rdat")





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
  
  #   xall <- xts(tt$vol,as.POSIXlt(tt$d, format="%Y-%m-%d %H:%M:%S"))
  #   chartSeries(xall)
  
  states=c(1,2)
  outputs=c(0,1)
  
  
  
  
  if (sum(train_tr$hit!="0")<4 | sum(train_tr$hit!="1")<4){ # si casi todos son 0 o 1
    pred=rep(round(mean(train_tr$hit)),nrow(train_te))  # la prediccion de test es el mismo valor que train
  }else{
    if(h2_1==2){
      #Vamos a aplicar HMM para detectar operating modes
      # extraemos los parametros del modelo usando Baum Welch y luego la secuencia de estados mas probables usdando viterbi
      # usamos 4 estados hidden y dos valores de salida 1-0
      #usamos el necesitar 10 muestars por cada parametro a calcular, con 4 estados y 2 salidas los parametros son 4*4+2*4=24 y 24*10 es menor que 328
      #set up the model
      set.seed(3)
      mod <- depmix(hit ~ 1, data=train_tr, family=multinomial("identity"),nstates=2)  #elegimos 4 estados ocultos
      #   mod <- depmix(vol ~ 1, data=train_tr, family=gaussian(),nstates=2)  #elegimos 4 estados ocultos
      
      #fit the model 
      f <- fit(mod,verbose=F)
      #     AIC(f);BIC(f)
      #check to see how the state transtion matricies and process mean/sd matches our sample data
      #     summary(f)
      #     f
      
      tranprob=t(f@trDens[1,,])
      emiprob=rbind(f@response[[1]][[1]]@parameters[[1]],
                    f@response[[2]][[1]]@parameters[[1]])
      
      #get the estimated state for each timestep 
      pos <- posterior(f)
      #     ggplot(train_tr, aes(x=hserie,y=vol)) + geom_line()+ geom_segment(aes(x = 1:(nrow(pos)), y = pos$state*1000, xend = 2:(nrow(pos)+1), yend = pos$state*1000),color=as.factor(pos$state))
      
      #   xall <- xts(train_tr$vol,as.POSIXlt(train_tr$d, format="%Y-%m-%d %H:%M:%S"))
      #   chartSeries(xall)
      #   addLines(v=which(pos[,1]==1)) #marcar punto re ruptura 1
      #   y0 = rep(0,nrow(train_tr))
      #   y1=y0; y1[pos$state==1]=1
      #   sfun1  = stepfun(1:(length(y0)-1), y1, f = 0)
      #   plot(sfun1,col=2)
      
      #Prediccion
      maxlag=33  #longitud de prediccion
      state.now=pos[nrow(pos),1]
      states.pred=NULL
      outs.pred=NULL
      for(i in 1:maxlag){
        state.now=sample(states,1,prob=tranprob[state.now,])
#         state.now
        states.pred=c(states.pred,state.now)
        out.now=sample(outputs,1,prob=emiprob[state.now,])
        outs.pred=c(outs.pred,out.now)
      }
      
      pred=outs.pred[c(18,33)]
    } else {
      # TODO
    }
  }

  pred_m[,1]=sid
  pred_m[,3]=pred
  pred.final = rbind(pred.final,pred_m)
}
colnames(pred.final) = c("iccid","sal","hit")
save(pred.final,file="FINAL.hmm2.pred.Rdat")


