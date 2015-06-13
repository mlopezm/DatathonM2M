
library(dplyr)
source("librarycode.R")

##############################################################################
#Mix usando medias
l.files=c("bayesglm4.pred.ts.reto.Rdat",
          "timeseries_arima2.pred.ts.reto.Rdat",
           "rf4.pred.ts.reto.Rdat"
          )

w=c(1,1,1)
# w=c(0,0,0,1)
# w= c(0.7949,
#      0.7624,
#      0.7952,
#      0.8043
#      )

w=w/sum(w)


load(l.files[1])
true.val=pred.ts.reto[,c(1,2)]


fore.val=matrix(0,nrow = nrow(pred.ts.reto) ,ncol = 2)
for (i in seq(length(l.files))){
 load(l.files[i])
 fore.val=fore.val+pred.ts.reto[,c(3,4)]*w[i]
}

#Transforma la fore.val a un valor de 0 o 1
fore.val[fore.val>0.5]=1
fore.val[fore.val<=0.5]=0


historyruns=NULL
for (k in seq(nrow(fore.val))){
    pred=fore.val[k,]
    tru=true.val[k,]
    vallossf=loss12(pred,tru)/length(pred)   #loss function
    
    sid=valsims[k]
    historyruns=rbind(historyruns,c( simid=sid,
                                     
                                     loss=vallossf,
                                     h2_1=2
    )
    )
}


historyrunsm= mean(historyruns[,2])
historyrunssd= sd(historyruns[,2])
historyrunsm
historyrunssd
save (historyruns,file="hr_mix1_all_2h_4hd_ts.reto.Rdat")


hr=as.data.frame(historyruns)
ggplot(hr, aes(x=loss)) + geom_density()
quantile(hr$loss, seq(0,1,by=0.1))
table(hr$loss)

sample_good=hr$simid[hr$loss==1]
sample_bad=hr$simid[hr$loss==-1]
save(sample_good,sample_bad,file="./sample_gb/samp_badgood_mix13.Rdat")





##############################################################################
#Mix usando mayority votes
l.files=c("bayesglm4.pred.ts.reto.Rdat",
          "timeseries_arima2.pred.ts.reto.Rdat",
          "rf4.pred.ts.reto.Rdat"
)

load(l.files[1])
true.val=pred.ts.reto[,c(1,2)]


fore.val=matrix(0,nrow = nrow(pred.ts.reto) ,ncol = 2)
for (i in seq(length(l.files))){
  load(l.files[i])
  fore.val=fore.val+pred.ts.reto[,c(3,4)]
}

#Transforma la fore.val de volumen en hit
thres=floor(length(l.files)/2)
fore.val[fore.val<=thres]=0
fore.val[fore.val>thres]=1



historyruns=NULL
for (k in seq(nrow(fore.val))){
  pred=fore.val[k,]
  tru=true.val[k,]
  vallossf=loss12(pred,tru)/length(pred)   #loss function
  
  sid=valsims[k]
  historyruns=rbind(historyruns,c( simid=sid,
                                   
                                   loss=vallossf,
                                   h2_1=2
  )
  )
}


historyrunsm= mean(historyruns[,2])
historyrunssd= sd(historyruns[,2])
historyrunsm
historyrunssd
save (historyruns,file="hr_mix1_all_2h_4hd_ts.reto.Rdat")


hr=as.data.frame(historyruns)
ggplot(hr, aes(x=loss)) + geom_density()
quantile(hr$loss, seq(0,1,by=0.1))
table(hr$loss)

sample_good=hr$simid[hr$loss==1]
sample_bad=hr$simid[hr$loss==-1]
save(sample_good,sample_bad,file="./sample_gb/samp_badgood_mix13.Rdat")




