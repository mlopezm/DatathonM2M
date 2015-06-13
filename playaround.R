old=read.csv("Datathon_ManuelLopezMartin.1.0.csv", sep=";")
new=read.csv("Datathon_ManuelLopezMartin.1.1.csv", sep=";")
sum(old$AFECTADA != new$AFECTADA)
sum(old$Periodo != new$Periodo)
sum(old$SIM != new$SIM)
load("sim.uwgh2_4_xtra.all.new.Rdat")
u=unique(old$SIM)
sum( u!= sim.ids)





#Crear los datos de test para el envio final
load("dat.ini.Rdat")
load("sim.uwgh2_4_xtra.all.new.Rdat") 
load("train_te.Rdat")

train_te_n=train_te[1,]
train_te_n[,]=0
train_te_n$d="2015-05-02 00:00:00"
save(train_te_n,file="train_te_n.Rdat")

temp=NULL
almacen=NULL
for (sid in valsims){
  temp=train_te
  temp$iccid=sid
  temp$apn=sim.p[sim.p$iccid==sid,"apn"]
  temp$rplan=sim.p[sim.p$iccid==sid,"assigned_rate_plan_id"]
  temp$acc=sim.p[sim.p$iccid==sid,"account_id"]
  almacen=rbind(almacen,temp)
}

train_te_t=almacen
train_te_t$sal=rep(c("A","B"),length(valsims))
save(train_te_t,file="train_te_final.Rdat")

load("train_te_final.Rdat")



#Juagar con ARIMAX
library(forecast)
# create some artifical data
modelfitsample <- data.frame(Customer_Visit=rpois(49,3000),Weekday=rep(1:7,7),
                             Christmas=c(rep(0,40),1,rep(0,8)),Day=1:49)

# Create matrix of numeric predictors
xreg <- cbind(Weekday=model.matrix(~as.factor(modelfitsample$Weekday)), 
              Day=modelfitsample$Day,
              Christmas=modelfitsample$Christmas)

# Remove intercept
xreg <- xreg[,-1]

# Rename columns
colnames(xreg) <- c("Mon","Tue","Wed","Thu","Fri","Sat","Day","Christmas")

# Variable to be modelled
visits <- ts(modelfitsample$Customer_Visit, frequency=7)

# Find ARIMAX model
modArima <- auto.arima(visits, xreg=xreg)

longilag=5
xreg2=xreg[1:1,]
pred2=forecast(modArima,h=longilag,xreg=xreg2)
#     plot(pred)
pred2=pred2$mean[1:longilag]
