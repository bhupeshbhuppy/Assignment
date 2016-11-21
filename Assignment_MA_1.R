setwd("C:\\Users\\bhupe\\Documents\\Applied_Trix")
library(xlsx)
dataf<-read.xlsx("data_ass.xlsx", sheetIndex = 1)
movingavg<-function(dataf, n, start_loc ){
    for(start_loc in 1:(length(dataf$Sales)-n+1)){
         dataf$ma12[n/2+start_loc-1] <-mean(dataf$Sales[start_loc:(n+start_loc-1)])
         }
     return(dataf)
   }
dataf<-movingavg(dataf,12,1)
dataf
movingavg<-function(dataf, n, start_loc ){
     for(start_loc in 1:(length(dataf$ma12)-n+1)){
           dataf$cma[n/2+start_loc] <-mean(dataf$ma12[start_loc:(n+start_loc-1)])
         }
     return(dataf)
   }
dataf<-movingavg(dataf,2,6)
dataf
dataf$SI<-dataf$Sales/dataf$cma
dataf
seasonalavg<-function(dataf){
  for(i in 1:length(dataf$SI)){
    mnth<-dataf$month[i]
    dataf$S[i]<-mean(dataf$SI[dataf$month==mnth], na.rm =TRUE)
  }
  return(dataf) 
}
dataf<-seasonalavg(dataf)
fac<-12/sum(dataf$S[1:12])
fac
dataf$S<-dataf$S*fac
#dataf$S<-dataf$S*12/sum(dataf$S[1:12])
dataf$ybys<-dataf$Sales/dataf$S
lm(dataf$ybys ~ dataf$Period..t., data=dataf)

dataf$t<-380.121+9.491*dataf$Period..t.
dataf$ts<-dataf$S*dataf$t
dataf$CI<-dataf$Sales/dataf$ts
movingavg<-function(dataf, n, start_loc ){
  for(start_loc in 1:(length(dataf$SI)-n+1)){
    dataf$ma3[n/2+start_loc] <-mean(dataf$CI[start_loc:(n+start_loc-1)])
  }
  return(dataf)
}
dataf<-movingavg(dataf,3,1)
dataf$I<-dataf$CI/dataf$ma3
dataf




