library(dplyr)
library(tidyr)
library(xlsx)
wpi_data_raw<-read.xlsx("rbi_wpi_data.xlsx", sheetIndex = 1)
#wpi_data<-tbl_df(wpi_data_raw)
colnames(wpi_data)[2]<-"prices"
wpi_data_ts<-ts(wpi_data[,2])
wpi_data_ts
wpi_data_ts_lag<-lag(wpi_data_ts)
wpi_data_ts_lag
wpi_data_ts_lagdiff<-wpi_data_ts-wpi_data_ts_lag
wpi_data_ts_lagdiff
wpi_data_ts_analysis<-data.frame(wpi_data$Date, wpi_data_ts, wpi_data_ts_lag, wpi_data_ts_lagdiff)
colnames(wpi_data_ts_analysis)<-c("Date", "p(t)", "p(t-1)", "p(t)-p(t-1)")
wpi_data_ts_analysis
wpi_data_ts_analysis<-mutate(wpi_data_ts_analysis,inflation=((`p(t)-p(t-1)`)/`p(t-1)`*100))
plot(wpi_data_ts_analysis$date, wpi_data_ts_analysis$inflation, typ="l")



