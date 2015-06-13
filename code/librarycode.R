
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}


sum_logic <- function(val){
  val[1] | val[2]
}

#loss fun valores logical
loss1 <- function(x1,x2){
  pos=sum(!xor(x1,x2),na.rm=T)
  neg=sum(xor(x1,x2),na.rm=T)
  negna=sum(is.na(xor(x1,x2)))*0.2
  return(pos-neg-negna)
}

#loss fun valores numeric
loss12 <- function(x1,x2){
  pos=sum(x1==x2,na.rm=T)
  neg=sum(x1!=x2,na.rm=T)
  negna=sum(is.na(x1==x2))*0.2
  return(pos-neg-negna)
}

#Transforma el time serie a formato wide en 1-horas intervalo
towidef1h <- function(sim.id) {
  # sim.id es un vector de valores
  
  #Crear data frame template que sera reusado por todos los SIMS para transformar sus datos a formato wide
  sim.uw.tplate= data.frame(iccid=0,d=0,wd=0,hd=0,hserie=1:diff.hour,day=0,h2d=0,vol=0,hit=F)
  h=h1
  for (i in seq(diff.hour)){
    sim.uw.tplate$d[i]=format(h,format="%Y-%m-%d %H:%M:%S")
    sim.uw.tplate$hd[i]=hour(h)
    sim.uw.tplate$wd[i]=wday(h)
    sim.uw.tplate$day[i]=day(h)   #Add dia del mes  a sim.uw, quitar al final
    sim.uw.tplate$h2d[i]=floor(sim.uw.tplate$hd[i]/2)  #Add numero de 2-hora del dia 
    h=h+3600
  }
  
  sim.uwg=NULL
  count=0
  for (i in sim.id){
    count=count+1
    print(paste(count,"-",i))
    t= sim.u %>% filter(iccid==i) %>% arrange(record_open_time) 
#     sec.start=as.numeric(strptime(t$record_open_time, "%Y-%m-%d %H:%M:%S") - h1,units="secs") #open time conexión de las muestras en sec desde h1
#     sec.end=sec.start+t$session_duration  #end time conexión de las muestras desde h1
#     t =t %>% mutate(sec.start,sec.end)
    ft=as.POSIXlt(t$record_open_time, format="%Y-%m-%d %H:%M:%S")
#     d.start=day(ft)  # hora del dia  y dia de comienzo y fin de conexion por muestra
#     d.end=day(ft+t$session_duration)
#     h.start=hour(ft)
#     h.end=hour(ft+t$session_duration)
#     rate=t$data_usage_raw_total/t$session_duration  #rate de bits/sec
#     t=t %>% mutate(d.start,d.end,h.start,h.end,rate)
    t=t %>% mutate(record_end_time=format(ft+t$session_duration,format="%Y-%m-%d %H:%M:%S")) #fecha de desconexión de la muestra
    
    #Crear formato wide de datos para SIM
    sim.uw=sim.uw.tplate
    sim.uw$iccid=i
    for (j in seq(nrow(t))){
      ts.t= trunc(strptime(t$record_open_time[j], "%Y-%m-%d %H:%M:%S"), units="hours")  # t start truncado
      te.t= trunc(strptime(t$record_end_time[j], "%Y-%m-%d %H:%M:%S"), units="hours")  # t end truncado
      if (te.t == t$record_end_time[j]) te.t=te.t-3600  # para el caso en que t end este al limite de la siguiente hora
      idx1=which(sim.uw$d ==format(ts.t,format="%Y-%m-%d %H:%M:%S"))  #indices de comienxzo y fin de actividad
      idx2=which(sim.uw$d ==format(te.t,format="%Y-%m-%d %H:%M:%S"))
      imp.vec=idx1:idx2  #vector de imputacion de actividad en sim.uw
      sim.uw[imp.vec,"vol"]=t[j,"data_usage_raw_total"]/length(imp.vec)  #reparte el data usage de manera proporcional entre las horas inciial y final
      sim.uw[imp.vec,"hit"]=T
    }
    
    sim.uwg=rbind(sim.uwg,sim.uw)
  }
  
  
  return(sim.uwg)
  
}



#Transforma el time serie a formato wide en 2-horas intervalo
towidef2h <- function(sim.id) {
  # sim.id es un vector de valores
  
  #Crear data frame template que sera reusado por todos los SIMS para transformar sus datos a formato wide
  sim.uw.tplate= data.frame(iccid=0,d=0,wd=0,hd=0,hserie=1:diff.hour,day=0,h2d=0,month=0,vol=0,hit=F)
  h=h1
  for (i in seq(diff.hour)){
    sim.uw.tplate$d[i]=format(h,format="%Y-%m-%d %H:%M:%S")
    sim.uw.tplate$hd[i]=hour(h)
    sim.uw.tplate$wd[i]=wday(h)
    sim.uw.tplate$month[i]=month(h)
    sim.uw.tplate$day[i]=day(h)   #Add dia del mes  a sim.uw
    sim.uw.tplate$h2d[i]=floor(sim.uw.tplate$hd[i]/2)  #Add numero de 2-hora del dia 
    h=h+3600
  }
  
  sim.uwg=NULL
  count=0
  for (i in sim.id){
    count=count+1
    print(paste(count,"-",i))
    t= sim.u %>% filter(iccid==i) %>%  arrange(record_open_time) 
    #     sec.start=as.numeric(strptime(t$record_open_time, "%Y-%m-%d %H:%M:%S") - h1,units="secs") #open time conexión de las muestras en sec desde h1
    #     sec.end=sec.start+t$session_duration  #end time conexión de las muestras desde h1
    #     t =t %>% mutate(sec.start,sec.end)
    ft=as.POSIXlt(t$record_open_time, format="%Y-%m-%d %H:%M:%S")
    #     d.start=day(ft)  # hora del dia  y dia de comienzo y fin de conexion por muestra
    #     d.end=day(ft+t$session_duration)
    #     h.start=hour(ft)
    #     h.end=hour(ft+t$session_duration)
    #     rate=t$data_usage_raw_total/t$session_duration  #rate de bits/sec
    #     t=t %>% mutate(d.start,d.end,h.start,h.end,rate)
    t=t %>% mutate(record_end_time=format(ft+t$session_duration,format="%Y-%m-%d %H:%M:%S")) #fecha de desconexión de la muestra
    
    #Crear formato wide de datos para SIM
    sim.uw=sim.uw.tplate
    sim.uw$iccid=i
    for (j in seq(nrow(t))){
      ts.t= trunc(strptime(t$record_open_time[j], "%Y-%m-%d %H:%M:%S"), units="hours")  # t start truncado
      te.t= trunc(strptime(t$record_end_time[j], "%Y-%m-%d %H:%M:%S"), units="hours")  # t end truncado
      if (te.t == t$record_end_time[j]) te.t=te.t-3600  # para el caso en que t end este al limite de la siguiente hora
      idx1=which(sim.uw$d ==format(ts.t,format="%Y-%m-%d %H:%M:%S"))  #indices de comienxzo y fin de actividad
      idx2=which(sim.uw$d ==format(te.t,format="%Y-%m-%d %H:%M:%S"))
      imp.vec=idx1:idx2  #vector de imputacion de actividad en sim.uw
      sim.uw[imp.vec,"vol"]=t[j,"data_usage_raw_total"]/length(imp.vec)  #reparte el data usage de manera proporcional entre las horas inciial y final
      sim.uw[imp.vec,"hit"]=T
    }
    
    #colapsar datos en dos horas
    vol=(sim.uw %>% group_by(month,day,h2d) %>% summarise(vol=sum(vol))) [,"vol"]
    hit=(sim.uw %>% group_by(month,day,h2d) %>% summarise(hit=sum_logic(hit))) [,"hit"]
    
    sim.uw=sim.uw[-(seq(0,to=nrow(sim.uw),by=2)),]
    sim.uw$vol=unlist(vol)
    sim.uw$hit=unlist(hit)
    sim.uw$hserie=1:nrow(sim.uw)
    
    sim.uwg=rbind(sim.uwg,sim.uw)
  }

  
  return(sim.uwg)
  
}




#Transforma el time serie a formato wide en 2-horas intervalo, añadiendo el intervalo de 4h tambien
towidef2_4h <- function(sim.id) {
  # sim.id es un vector de valores
  
  #Crear data frame template que sera reusado por todos los SIMS para transformar sus datos a formato wide
  sim.uw.tplate= data.frame(iccid=0,d=0,wd=0,hd=0,hserie=1:diff.hour,day=0,h2d=0,h4d=0,month=0,vol=0,hit=F)
  h=h1
  for (i in seq(diff.hour)){
    sim.uw.tplate$d[i]=format(h,format="%Y-%m-%d %H:%M:%S")
    sim.uw.tplate$hd[i]=hour(h)
    sim.uw.tplate$wd[i]=wday(h)
    sim.uw.tplate$month[i]=month(h)
    sim.uw.tplate$day[i]=day(h)   #Add dia del mes  a sim.uw
    sim.uw.tplate$h2d[i]=floor(sim.uw.tplate$hd[i]/2)  #Add numero de 2-hora del dia 
    sim.uw.tplate$h4d[i]=floor(sim.uw.tplate$h2d[i]/2)  #Add numero de 4-hora del dia 
    h=h+3600
  }
  
  sim.uwg=NULL
  count=0
  for (i in sim.id){
    count=count+1
    print(paste(count,"-",i))
    t= sim.u %>% filter(iccid==i) %>%  arrange(record_open_time) 
    #     sec.start=as.numeric(strptime(t$record_open_time, "%Y-%m-%d %H:%M:%S") - h1,units="secs") #open time conexión de las muestras en sec desde h1
    #     sec.end=sec.start+t$session_duration  #end time conexión de las muestras desde h1
    #     t =t %>% mutate(sec.start,sec.end)
    ft=as.POSIXlt(t$record_open_time, format="%Y-%m-%d %H:%M:%S")
    #     d.start=day(ft)  # hora del dia  y dia de comienzo y fin de conexion por muestra
    #     d.end=day(ft+t$session_duration)
    #     h.start=hour(ft)
    #     h.end=hour(ft+t$session_duration)
    #     rate=t$data_usage_raw_total/t$session_duration  #rate de bits/sec
    #     t=t %>% mutate(d.start,d.end,h.start,h.end,rate)
    t=t %>% mutate(record_end_time=format(ft+t$session_duration,format="%Y-%m-%d %H:%M:%S")) #fecha de desconexión de la muestra
    
    #Crear formato wide de datos para SIM
    sim.uw=sim.uw.tplate
    sim.uw$iccid=i
    for (j in seq(nrow(t))){
      ts.t= trunc(strptime(t$record_open_time[j], "%Y-%m-%d %H:%M:%S"), units="hours")  # t start truncado
      te.t= trunc(strptime(t$record_end_time[j], "%Y-%m-%d %H:%M:%S"), units="hours")  # t end truncado
      if (te.t == t$record_end_time[j]) te.t=te.t-3600  # para el caso en que t end este al limite de la siguiente hora
      idx1=which(sim.uw$d ==format(ts.t,format="%Y-%m-%d %H:%M:%S"))  #indices de comienxzo y fin de actividad
      idx2=which(sim.uw$d ==format(te.t,format="%Y-%m-%d %H:%M:%S"))
      imp.vec=idx1:idx2  #vector de imputacion de actividad en sim.uw
      sim.uw[imp.vec,"vol"]=t[j,"data_usage_raw_total"]/length(imp.vec)  #reparte el data usage de manera proporcional entre las horas inciial y final
      sim.uw[imp.vec,"hit"]=T
    }
    
    #colapsar datos en dos horas
    vol=(sim.uw %>% group_by(month,day,h2d) %>% summarise(vol=sum(vol))) [,"vol"]
    hit=(sim.uw %>% group_by(month,day,h2d) %>% summarise(hit=sum_logic(hit))) [,"hit"]
    
    sim.uw=sim.uw[-(seq(0,to=nrow(sim.uw),by=2)),]
    sim.uw$vol=unlist(vol)
    sim.uw$hit=unlist(hit)
    sim.uw$hserie=1:nrow(sim.uw)
    
    sim.uwg=rbind(sim.uwg,sim.uw)
  }
  
  
  return(sim.uwg)
  
}



#Transforma el time serie a formato wide en 2-horas intervalo, añadiendo el intervalo de 4h tambien
towidef2_4h_xtra <- function(sim.id) {
  # sim.id es un vector de valores
  
  #Crear data frame template que sera reusado por todos los SIMS para transformar sus datos a formato wide
  sim.uw.tplate= data.frame(iccid=0,d=0,wd=0,hd=0,hserie=1:diff.hour,day=0,h2d=0,h4d=0,month=0,acc=0, apn=0,rplan=0,vol=0,hit=F)
  h=h1
  for (i in seq(diff.hour)){
    sim.uw.tplate$d[i]=format(h,format="%Y-%m-%d %H:%M:%S")
    sim.uw.tplate$hd[i]=hour(h)
    sim.uw.tplate$wd[i]=wday(h)
    sim.uw.tplate$month[i]=month(h)
    sim.uw.tplate$day[i]=day(h)   #Add dia del mes  a sim.uw
    sim.uw.tplate$h2d[i]=floor(sim.uw.tplate$hd[i]/2)  #Add numero de 2-hora del dia 
    sim.uw.tplate$h4d[i]=floor(sim.uw.tplate$h2d[i]/2)  #Add numero de 4-hora del dia 
    h=h+3600
  }
  
  sim.uwg=NULL
  count=0
  for (i in sim.id){
    count=count+1
    print(paste(count,"-",i))
    t= sim.u %>% filter(iccid==i) %>%  arrange(record_open_time) 
    #     sec.start=as.numeric(strptime(t$record_open_time, "%Y-%m-%d %H:%M:%S") - h1,units="secs") #open time conexión de las muestras en sec desde h1
    #     sec.end=sec.start+t$session_duration  #end time conexión de las muestras desde h1
    #     t =t %>% mutate(sec.start,sec.end)
    ft=as.POSIXlt(t$record_open_time, format="%Y-%m-%d %H:%M:%S")
    #     d.start=day(ft)  # hora del dia  y dia de comienzo y fin de conexion por muestra
    #     d.end=day(ft+t$session_duration)
    #     h.start=hour(ft)
    #     h.end=hour(ft+t$session_duration)
    #     rate=t$data_usage_raw_total/t$session_duration  #rate de bits/sec
    #     t=t %>% mutate(d.start,d.end,h.start,h.end,rate)
    t=t %>% mutate(record_end_time=format(ft+t$session_duration,format="%Y-%m-%d %H:%M:%S")) #fecha de desconexión de la muestra
    
    #Crear formato wide de datos para SIM
    sim.uw=sim.uw.tplate
    sim.uw$iccid=i
    for (j in seq(nrow(t))){
      ts.t= trunc(strptime(t$record_open_time[j], "%Y-%m-%d %H:%M:%S"), units="hours")  # t start truncado
      te.t= trunc(strptime(t$record_end_time[j], "%Y-%m-%d %H:%M:%S"), units="hours")  # t end truncado
      if (te.t == t$record_end_time[j]) te.t=te.t-3600  # para el caso en que t end este al limite de la siguiente hora
      idx1=which(sim.uw$d ==format(ts.t,format="%Y-%m-%d %H:%M:%S"))  #indices de comienxzo y fin de actividad
      idx2=which(sim.uw$d ==format(te.t,format="%Y-%m-%d %H:%M:%S"))
      imp.vec=idx1:idx2  #vector de imputacion de actividad en sim.uw
      sim.uw[imp.vec,"vol"]=t[j,"data_usage_raw_total"]/length(imp.vec)  #reparte el data usage de manera proporcional entre las horas inciial y final
      sim.uw[imp.vec,"hit"]=T
    }
    
    #guardar datos de cuenta, apn y rate plan
    x=sim.p %>% filter(iccid == i)
    sim.uw$apn= x[,"apn"]
    sim.uw$rplan= x[,"assigned_rate_plan_id"]
    sim.uw$acc= x[,"account_id"]
    
    #colapsar datos en dos horas
    vol=(sim.uw %>% group_by(month,day,h2d) %>% summarise(vol=sum(vol))) [,"vol"]
    hit=(sim.uw %>% group_by(month,day,h2d) %>% summarise(hit=sum_logic(hit))) [,"hit"]
    
    sim.uw=sim.uw[-(seq(0,to=nrow(sim.uw),by=2)),]
    sim.uw$vol=unlist(vol)
    sim.uw$hit=unlist(hit)
    sim.uw$hserie=1:nrow(sim.uw)
    
    sim.uwg=rbind(sim.uwg,sim.uw)
  }
  
  
  return(sim.uwg)
  
}



## funcion que añade la fecha y hora para el fichero de salida
dateTimeAppend <- function(str1, str2, str3,date.format ="%Y%m%d%H%M%S") {
  #stopifnot(is.character(str1))
  return(sprintf("%s_%s_%s.%s", str1,str2, format(Sys.time(), date.format),str3))
}



## funcion RMSE
rmse<- function(y1,y2){ # dos vectores
  len=length(y1)
  res= sqrt(sum((y1-y2)^2)/len)
  return(res)
}




# sigmoid <- function(x,a=10,b=0.5) {
#   1 / ( 1 + exp(-a*(x-b) ))
# }




#plot(sigmoid,0,1)
# x=seq(0,1,length.out = 100)
# y=sigmoid(x,50,0.5)
# plot(x,y)
