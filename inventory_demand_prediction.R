# Inventory Demand Prediction - model training script
pkgs<-c("tidyverse","lubridate","zoo","randomForest","scales");suppressPackageStartupMessages(lapply(pkgs,library,character.only=TRUE));set.seed(42)
raw<-read.csv("train.csv",stringsAsFactors=FALSE);raw$date<-as.Date(raw$date)
df<-raw%>%filter(!is.na(sales))%>%mutate(sales=pmax(sales,0),store=as.integer(store),item=as.integer(item))%>%arrange(store,item,date)
last_date<-max(df$date);TEST_DAYS<-90;split_date<-last_date-TEST_DAYS;features<-c("store","item","lag_7","lag_14","lag_28","roll_7","roll_28","dow","month","year","doy","weekend")
feat<-df%>%group_by(store,item)%>%arrange(date,.by_group=TRUE)%>%mutate(lag_7=lag(sales,7),lag_14=lag(sales,14),lag_28=lag(sales,28),roll_7=rollapplyr(lag(sales,7),7,mean,fill=NA),roll_28=rollapplyr(lag(sales,7),28,mean,fill=NA))%>%ungroup()%>%mutate(dow=wday(date,week_start=1),month=month(date),year=year(date),doy=yday(date),weekend=as.integer(dow>=6))%>%drop_na()
train<-feat%>%filter(date<=split_date);test<-feat%>%filter(date>split_date);train_s<-train%>%slice_sample(n=min(60000,nrow(train)));rf<-randomForest(x=train_s[,features],y=train_s$sales,ntree=100);test$pred<-predict(rf,test[,features]);err_tbl<-test%>%group_by(store,item)%>%summarise(err_sd=sd(sales-pred),.groups="drop");nz<-test$sales>0
cache<-list(rf=rf,err_tbl=err_tbl,rmse=sqrt(mean((test$sales-test$pred)^2)),mape=mean(abs(test$sales[nz]-test$pred[nz])/test$sales[nz])*100);saveRDS(cache,"rf_cache.rds");print(cache[c("rmse","mape")])
