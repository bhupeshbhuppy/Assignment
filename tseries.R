library(tseries)
library(xtable)
data("NelPlo")
## Create a random walk process
set.seed(99)
e<-rnorm(100)
yt<-0
for(t in 2:100){
  yt[t]= yt[t-1]+e[t]
}
yt<-ts(yt)
## To CHeck the original series
log_NelPlo<-log(NelPlo)
tbl<-data.frame()
cnme<-colnames(NelPlo)
a<-acf(yt)
tbl[1,1]<-"Random Walk"
tbl[1,2]<-100
for(i in 1:length(a)){
  tbl[1,i+2]<-a[i]
}
for(j in 1:length(cnme)){
  b<-acf(log_NelPlo[,j], na.action = na.pass, plot = FALSE)
  #prd<-range(time(log_NelPlo[,j]))
  tbl[j+1,1]<-cnme[j+1]
  #tbl[j,2]<-paste(prd[1],prd[2],sep="-")
  tbl[j+1,2]<-length(which(!is.na(log_NelPlo[,j])))
  for(i in 1:(length(b))){
    tbl[j+1,i+2]<-b[i]
  }
}
colnames(tbl)<-c("series","T","r1","r2","r3","r4","r5","r6")
xtable(tbl, caption="Table 1: Sample autocorrelation of the series")
for(j in 1:14){
  plot.ts(log_NelPlo[,j], ylab=cnme[j] , main=paste("Fig 1.1: Actual series of", cnme[j]))
  plot.ts(acf(log_NelPlo[,j]),ylab=cnme[j],main="Fig 1.2: ACF of the Series" )
}

lag_NelPlo<-lag(log_NelPlo,-1)
lag_yt<-lag(yt,-1)
diff_NelPlo<-NelPlo-lag_NelPlo
diff_yt<-yt-lag_yt
tbl_firstdiff<-data.frame()
tbl_firstdiff[1,1]<-"Time aggegrated Random Walk"
tbl_firstdiff[1,2]<-100
a<-acf(diff_yt, plot=FALSE)
for(i in 1:length(a)){
  tbl_firstdiff[1,i+2]<-a[i]
}
for(j in 1:length(cnme)){
  b<-acf(diff_NelPlo[,j], na.action = na.pass, plot = FALSE)
  tbl_firstdiff[j+1,1]<-cnme[j]
  tbl_firstdiff[j+1,2]<-length(which(!is.na(diff_NelPlo[,j])))
  for(i in 1:(length(b))){
    tbl_firstdiff[j+1,i+2]<-b[i]
  }
}
colnames(tbl_firstdiff)<-c("series","T","r1","r2","r3","r4","r5","r6")
tbl_firstdiff
##xtable(tbl_firstdiff, caption="Table 2: Sample autocorrelation of the first difference of series")


residual_NelPlo<-data.frame()
for(j in 1:length(cnme)){
  t<-1:length(NelPlo[,j])
  model<- lm(NelPlo[,j] ~ t)
  r<-resid(model)
  for(i in 1:(length(r))){
    residual_NelPlo[i,j]<-r[i]
  }
}
tbl_timetrend<-data.frame()
res_yt<-resid(lm(yt ~ c(1:100)))
a<-acf(res_yt, plot= FALSE)
tbl_timetrend[1,1]<-"Detrended Random Walk"
tbl_timetrend[1,2]<-100
for(i in 1:length(a)){
  tbl_timetrend[1,i+2]<-a[i]
}

for(j in 1:length(cnme)){
  b<-acf(residual_NelPlo[,j], na.action = na.pass, plot = FALSE)
  tbl_timetrend[j+1,1]<-cnme[j]
  tbl_timetrend[j+1,2]<-length(which(!is.na(residual_NelPlo[,j])))
  for(i in 1:(length(b))){
    tbl_timetrend[j+1,i+2]<-b[i]
  }
} 
colnames(tbl_timetrend)<-c("series","T","r1","r2","r3","r4","r5","r6")
tbl_timetrend
#xtable(tbl_timetrend, caption="Table 3: Sample autocorrelation of the deviation from the time trend")