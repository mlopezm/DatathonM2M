
source("librarycode.R")
# source("loaddat.R")
# source("preparedat.R")

load("dat.ini.Rdat")
load("sim.uwgh2_4.all.new.Rdat")
library(lubridate)
library(ggplot2)
library(dplyr)
library(entropy)
library(xts)
library(forecast)
library(quantmod)
library(corrplot)



set.seed(126)
#Extraer los datos en wide format para varias sim y luego hacer plot de su actividad
sim.uwg=towidef2h(sample(1:n.sims, 6, replace=F))  # transformamos a wide format


#Plot some activities
# plot(sim.uw$hserie,sim.uw$vol, main=paste("vol plot sim: ",sim.id))
# plot(sim.uw$hserie,sim.uw$vol,col=sim.uw$hd,main=paste("vol plot sim: ",sim.id))
# plot(sim.uw$hserie,sim.uw$vol,col=sim.uw$wd,main=paste("vol plot sim: ",sim.id))
# plot(sim.uw$hserie,sim.uw$hit,col=sim.uw$wd,main=paste("hit plot sim: ",sim.id))



#Plots de properties
res=sim.prop %>% arrange(iccid)  %>% group_by(account_id,iccid) %>% summarise(n=n())
res=sim.prop %>% arrange(iccid)  %>% group_by(apn,account_id) %>% summarise(n=n())
res=sim.prop %>% arrange(iccid)  %>% group_by(iccid,account_id) %>% summarise(n=n())
res=sim.prop %>% arrange(iccid)  %>% group_by(op_network_id,iccid) %>% summarise(n=n())  
res=sim.prop %>% arrange(iccid)  %>% group_by(apn,op_network_id) %>% summarise(n=n())  
res=sim.prop %>% arrange(iccid)  %>% group_by(assigned_rate_plan_id,iccid) %>% summarise(n=n()) 
res=sim.prop %>% arrange(iccid)  %>% group_by(assigned_rate_plan_id,account_id) %>% summarise(n=n())
sum(res[,3]!=1)
pairs(sim.prop)
pairs(sim.p.import)
plotmatrix(sim.prop)
qplot(account_id,iccid,data=sim.prop,main=paste("sim properties plot"))
qplot(iccid,account_id,data=sim.prop,main=paste("sim properties plot"))
qplot(account_id, as.factor(import),data=sim.p.import,geom=c("boxplot"),colour=(assigned_rate_plan_id),main=paste("Importancia samples"))
ggplot(sim.p.import, aes(as.factor(import), fill=cut)) + geom_bar(position="dodge")


#Plot un SIM particular
vals=unique(sim.uwg$iccid)
id=1  #id del sim
temp= sim.uwg %>% filter(iccid==vals[id])
vals=13;id=1015;temp= sim.uwg %>% filter(iccid==vals[id]); 
qplot(hserie,vol,data=temp, geom=c("line"),main=paste("vol plot sim: ",vals[id]))
qplot(hserie,vol,data=temp, colour=as.factor(wd),main=paste("vol plot sim: ",vals[id]))
x <- xts(temp$vol,as.POSIXlt(temp$d, format="%Y-%m-%d %H:%M:%S"))
chartSeries(x)
qplot(wd,vol,data=temp, colour=as.factor(h2d),main=paste("vol plot sim: ",vals[id]))
qplot(hserie,hit,data=temp,colour=as.factor(wd),main=paste("hit plot sim: ",vals[id]))
qplot(hserie,vol,data=temp,geom=c("point","smooth"),main=paste("vol plot sim: ",vals[id]))
qplot(hserie,vol,data=temp,geom=c("path","smooth"),main=paste("vol plot sim: ",vals[id]))
qplot(as.factor(wd),vol,data=temp,geom=c("boxplot"),main=paste("vol plot sim: ",vals[id]))
qplot(as.factor(wd),hit,data=temp,geom=c("jitter"),main=paste("hit plot sim: ",vals[id]))
qplot(as.factor(h2d),vol,data=temp,geom=c("boxplot"),colour=as.factor(wd),main=paste("vol plot sim: ",vals[id]))
qplot(as.factor(h2d),hit,data=temp,geom=c("boxplot"),colour=as.factor(wd),main=paste("hit plot sim: ",vals[id]))
qplot(as.factor(hd),vol,data=temp,geom=c("boxplot"),colour=as.factor(wd),main=paste("vol plot sim: ",vals[id]))
qplot(as.factor(hd),hit,data=temp,geom=c("boxplot"),colour=as.factor(wd),main=paste("hit plot sim: ",vals[id]))
qplot(vol,data=temp,geom=c("density"),colour=as.factor(wd),main=paste("vol plot sim: ",vals[id]))
qplot(vol,data=temp,geom=c("density"),colour=as.factor(hd),main=paste("vol plot sim: ",vals[id]))

m=temp %>%  group_by(wd,h2d) %>% summarise(hit_count=sum(hit))
ggplot(m, aes(x=wd, y=h2d, fill=(hit_count>0))) + geom_tile()  #checkerboard graph of activity

table(temp$hit)
entropy(temp$vol)



#Time series
vals=4479;id=1;temp= sim.uwg %>% filter(iccid==vals[id]);
# tsvol <- ts(temp$vol, start=c(2009, 1), end=c(2014, 12), frequency=12) 
fd=as.POSIXlt(temp$d, format="%Y-%m-%d %H:%M:%S")
x <- xts(temp$vol,as.POSIXlt(temp$d, format="%Y-%m-%d %H:%M:%S"))
chartSeries(x)
addLines(v=which(wday(fd)==1)) #marcar domingo
plot(x)
# subset by time of day
y <- x["T06:00/T08:00"]
plot(y)
chartSeries(y)

fit <- stl(x, s.window="period")
plot(fit)

fit <- HoltWinters(x)
a=periodicity(x)$scale
a=as.ts(x)
fit <- HoltWinters(a)


ets(x)  


#Correlacion
M <- cor(sim.prop)
corrplot(M, method = "ellipse",order = "hclust")
x=sim.uwg %>% select(iccid,wd,hd,vol)
x=data.matrix(x)
M <- cor(x)
corrplot(M, method = "ellipse",order = "hclust")
