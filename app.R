# Inventory Demand Prediction - Shiny App (BDA Mini Project)
suppressPackageStartupMessages({library(shiny);library(tidyverse);library(lubridate);library(zoo);library(randomForest)})
set.seed(42)
if(file.exists("train.csv")){raw<-read.csv("train.csv",stringsAsFactors=FALSE);raw$date<-as.Date(raw$date)}else{
 grid<-expand.grid(date=seq(as.Date("2013-01-01"),as.Date("2017-12-31"),by="day"),store=1:5,item=1:20)
 base<-runif(100,15,60);grid$base<-base[(grid$store-1)*20+grid$item];dow_eff<-c(.85,.85,.9,.95,1.05,1.25,1.2)
 grid$sales<-rpois(nrow(grid),grid$base*(1+.3*sin(2*pi*yday(grid$date)/365))*dow_eff[wday(grid$date,week_start=1)]*(1+.08*(year(grid$date)-2013)))
 raw<-grid[,c("date","store","item","sales")]
}
df<-raw%>%filter(!is.na(sales))%>%mutate(sales=pmax(sales,0),store=as.integer(store),item=as.integer(item))%>%arrange(store,item,date)
last_date<-max(df$date);TEST_DAYS<-90;split_date<-last_date-TEST_DAYS;stores<-sort(unique(df$store));items<-sort(unique(df$item))
features<-c("store","item","lag_7","lag_14","lag_28","roll_7","roll_28","dow","month","year","doy","weekend")
if(file.exists("rf_cache.rds")){cache<-readRDS("rf_cache.rds")}else{
 feat<-df%>%group_by(store,item)%>%arrange(date,.by_group=TRUE)%>%mutate(lag_7=lag(sales,7),lag_14=lag(sales,14),lag_28=lag(sales,28),roll_7=rollapplyr(lag(sales,7),7,mean,fill=NA),roll_28=rollapplyr(lag(sales,7),28,mean,fill=NA))%>%ungroup()%>%mutate(dow=wday(date,week_start=1),month=month(date),year=year(date),doy=yday(date),weekend=as.integer(dow>=6))%>%drop_na()
 train<-feat%>%filter(date<=split_date);test<-feat%>%filter(date>split_date);train_s<-train%>%slice_sample(n=min(60000,nrow(train)));rf<-randomForest(x=train_s[,features],y=train_s$sales,ntree=100);test$pred<-predict(rf,test[,features])
 err_tbl<-test%>%group_by(store,item)%>%summarise(err_sd=sd(sales-pred),.groups="drop");nz<-test$sales>0
 cache<-list(rf=rf,err_tbl=err_tbl,rmse=sqrt(mean((test$sales-test$pred)^2)),mape=mean(abs(test$sales[nz]-test$pred[nz])/test$sales[nz])*100);saveRDS(cache,"rf_cache.rds")
}
ui<-fluidPage(titlePanel("Inventory Demand Prediction"),sidebarLayout(sidebarPanel(h4("Product Details"),selectInput("store","Store",choices=stores),selectInput("item","Item",choices=items),sliderInput("horizon","Forecast period (days)",7,30,14,1),h4("Inventory Settings"),numericInput("lead_time","Supplier lead time (days)",7,1,30),selectInput("service","Service level",choices=c("90%"=1.28,"95%"=1.65,"99%"=2.33),selected=1.65),numericInput("stock","Current stock (units)",100,min=0),actionButton("go","Predict Demand",class="btn-primary")),mainPanel(h2("Prediction"),uiOutput("recommendation"),br(),h4("Key Numbers"),tableOutput("summary_tbl"),h4("Demand Forecast"),plotOutput("forecast_plot",height="320px"),h4("Daily Forecast Table"),tableOutput("forecast_tbl"),br(),p(textOutput("model_info")))))
server<-function(input,output,session){
 result<-eventReactive(input$go,{st<-as.integer(input$store);it<-as.integer(input$item);H<-input$horizon;L<-input$lead_time;z<-as.numeric(input$service);hist<-df%>%filter(store==st,item==it)%>%arrange(date);validate(need(nrow(hist)>60,"Not enough history for this store-item."));n<-nrow(hist);v<-c(hist$sales,rep(NA_real_,H));fdates<-max(hist$date)+seq_len(H)
  for(h in seq_len(H)){t<-n+h;d<-fdates[h];wd<-wday(d,week_start=1);nd<-data.frame(store=st,item=it,lag_7=v[t-7],lag_14=v[t-14],lag_28=v[t-28],roll_7=mean(v[(t-13):(t-7)]),roll_28=mean(v[(t-34):(t-7)]),dow=wd,month=month(d),year=year(d),doy=yday(d),weekend=as.integer(wd>=6));v[t]<-max(0,as.numeric(predict(cache$rf,nd[,features])))}
  fc<-data.frame(date=fdates,forecast=v[(n+1):(n+H)]);err_sd<-cache$err_tbl%>%filter(store==st,item==it)%>%pull(err_sd);if(length(err_sd)==0||is.na(err_sd))err_sd<-sd(hist$sales)*.3;avg_daily<-mean(fc$forecast);safety<-ceiling(z*err_sd*sqrt(L));rop<-ceiling(avg_daily*L+safety);total_h<-ceiling(sum(fc$forecast));order_qty<-max(0,total_h+safety-input$stock);list(fc=fc,hist=hist,avg_daily=avg_daily,safety=safety,rop=rop,total_h=total_h,order_qty=order_qty,stock=input$stock,H=H,st=st,it=it)})
 output$recommendation<-renderUI({r<-result();if(r$stock<=r$rop)div(style="padding:20px;border:2px solid #dc3545;border-radius:10px;",h2("Reorder Now"),p(paste0("Current stock (",r$stock,") is at or below the reorder point (",r$rop,"). Suggested order quantity: ",r$order_qty," units to cover the next ",r$H," days.")))else div(style="padding:20px;border:2px solid #198754;border-radius:10px;",h2("Stock is Sufficient"),p(paste0("Current stock (",r$stock,") is above the reorder point (",r$rop,"). No order needed yet.")))})
 output$summary_tbl<-renderTable({r<-result();data.frame(Metric=c("Average daily demand (forecast)",paste0("Total demand, next ",r$H," days"),"Safety stock","Reorder point","Current stock"),Value=c(round(r$avg_daily,1),r$total_h,r$safety,r$rop,r$stock))})
 output$forecast_plot<-renderPlot({r<-result();h<-r$hist%>%slice_tail(n=60)%>%select(date,sales);ggplot()+geom_line(data=h,aes(date,sales,colour="History"),linewidth=.7)+geom_line(data=r$fc,aes(date,forecast,colour="Forecast"),linewidth=.9,linetype="dashed")+scale_colour_manual(values=c(History="steelblue",Forecast="red"))+labs(title=paste0("Store ",r$st," - Item ",r$it),x="Date",y="Units per day",colour="")+theme_minimal(base_size=13)})
 output$forecast_tbl<-renderTable({r<-result();data.frame(Date=format(r$fc$date,"%d %b %Y"),Day=format(r$fc$date,"%A"),Forecast_units=round(r$fc$forecast,1))})
 output$model_info<-renderText({paste0("Model: Random Forest trained on store-item sales. Accuracy on the last ",TEST_DAYS," days: MAPE = ",round(cache$mape,1),"%, RMSE = ",round(cache$rmse,1)," units.")})
}
shinyApp(ui=ui,server=server)
