############ initialization ###################### 
library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(magrittr) 
library(png) 
library(lubridate) 
library(wesanderson)
library(viridis)
library(kableExtra)

# 5 significant digits
options(digits = 5)

######################## Data Manipulation and Exploratory Analysis #######################

# Original Walmart Stores Sales  dataset location:
# https://www.kaggle.com/vik2012kvs/walmart-dataretail-analysis/



# New location on GitHub
url<- "https://raw.githubusercontent.com/dongjiecheng/walmart_Store_Sales/main/Walmart_Store_sales.csv"

#read in data
dat<-read_csv(url)

#glimpse
glimpse(dat)

###################### Data Cleaning and Manipulation

#class of variables
lapply(dat, class)

# check Date formats
unique(dat$Date)

#check NA
sapply(dat, function(x)
  sum(is.na(x)))


# sales histogram plots
dat%>%group_by(Weekly_Sales)%>%
  ggplot(aes(Weekly_Sales))+geom_histogram(fill="blue",color="red")+scale_x_log10()+  
  scale_fill_brewer(palette = "Spectral")+guides(color = "none")+
  labs(title = "Histogram of Weekly_Sales", y = "count", x = "weekly sales($)", 
       caption="Walmart Stores Sales")


# Date format conversion and differentiate normal day and holiday
ndat<-dat%>%mutate(Date=dmy(dat$Date),nDate=as.numeric(Date))%>%mutate(
  Holiday_Flag=factor(Holiday_Flag),
  Day_class=ifelse(Holiday_Flag==1,"Holiday","Normalday")
)

# setup holidays
index<-as.character(ndat$Date)%in%c("2010-02-12","2011-02-11","2012-02-10","2013-02-08")
ndat$Day_class[index]<-"Super_Bowl"
index<-as.character(ndat$Date)%in%c("2010-09-10","2011-09-09","2012-09-07","2013-09-06")
ndat$Day_class[index]<-"Labor_Day"
index<-as.character(ndat$Date)%in%c("2010-11-26","2011-11-25","2012-11-23","2013-11-29")
ndat$Day_class[index]<-"Thanksgiving"
index<-as.character(ndat$Date)%in%c("2010-12-31","2011-12-30","2012-12-28","2013-12-27")
ndat$Day_class[index]<-"Christmas"

# unique dates count
subset(ndat, !duplicated(subset(ndat, select=Date)))%>%count(Day_class)%>% knitr::kable(caption = "Date counts")%>%
  column_spec(1, width = "3cm")%>%column_spec(2, width = "3cm")%>%
  kable_styling(latex_options = c("striped", "center"))

####################### Exploratory Analysis
#check date's NA
sum(is.na(ndat$Date))

# Check days
ndat%>%group_by(Store)%>%summarise(sum=n())%>%.$sum

# how many week days 
ndat%>%mutate(weekday=weekdays(Date))%>%summarize(week_days=unique(weekday))

# check date distribution

ndat%>%group_by(Date)%>%ggplot(aes(Date,Store,color=Day_class))+geom_point(size=0.01)+
  labs(title = "Map of dates", y = "store", x = "dates", caption="Walmart Stores Sales")

### Task 1 and 2: Store with maximum sales and store with maximum standard deviation and its coefficient of mean

# store total sales plot
ndat%>%group_by(Store)%>%summarize(whole_sale=sum(Weekly_Sales))%>%
  ggplot() +
  geom_line(aes(Store,whole_sale),color="blue")+
  geom_point(aes(Store,whole_sale), color="blue")+
  labs(title = "Plot of total store sales", y = "sales", x = "store number",
       caption="Walmart Stores Sales")

# store sales mean errorbar plot 
ndat%>%group_by(Store)%>%summarize(sale_mean=mean(Weekly_Sales),
                                   std=sd(Weekly_Sales),n=n())%>%
  mutate(low=sale_mean-std/sqrt(n),high=sale_mean+std/sqrt(n))%>%
  ggplot(aes(Store,sale_mean))+
  geom_point(color="red",size=0.8)+
  geom_errorbar(aes(ymin=low, ymax=high), color="blue", width=.9, 
                position=position_dodge(.9))+
  labs(title = "Bar plot of store sales", y = "store sales mean", 
       x = "store number", caption="Walmart Stores Sales")

# store sales standard deviation and standard errors line plots
ndat%>%group_by(Store)%>%summarize(sale_mean=mean(Weekly_Sales),std=sd(Weekly_Sales),n=n())%>%
  mutate(ste=std/sqrt(n))%>%
  ggplot() +
  geom_line(aes(Store,std,color="std"))+geom_point(aes(Store,std,color="std"))+
  geom_line(aes(Store,ste,color="ste"))+geom_point(aes(Store,ste,color="ste"))+
  scale_color_discrete(name="Errors",labels=c("Standard deviation","standard error"))+
  labs(title = "Plot of standard deviation and standard error", 
       y = "error", x = "store number", caption="Walmart Stores Sales")

# store with maximum total sales
mTotal_store<-ndat%>%group_by(Store)%>%summarize(whole_sale=sum(Weekly_Sales),
                                                 sale_mean=mean(Weekly_Sales))%>%
  summarize(max_whole_sale=max(whole_sale),
            store_max_whole_sale=Store[which.max(whole_sale)])
mTotal_store

# store with maximum mean sales  
mMean_store<-ndat%>%group_by(Store)%>%summarize(sale_mean=mean(Weekly_Sales),
                                                std=sd(Weekly_Sales))%>%
  summarize(max_mean=(max(sale_mean)),store_max_mean=Store[which.max(sale_mean)])
mMean_store

# store with maximum standard deviation and its mean
mStd_store<-ndat%>%group_by(Store)%>%summarize(sale_mean=mean(Weekly_Sales),
                                               std=sd(Weekly_Sales))%>%
  summarize(max_standard_deviation=(max(std)),
            store_max_standard_deviation=which.max(std),
            store_mean=sale_mean[which.max(std)] )
mStd_store



### Task 3: Store(s) with good quarterly growth in Q3, 2012

# Q3 2012 growth
# all Q 2 & 3 2012 sales
q2_3_sales_all<-ndat%>%filter(Date>"2012-03-31"&Date<"2012-10-01")%>%
  mutate(quarter=ifelse(Date>"2012-06-30",3,2))

# count weeks
q2_3_sales_all%>%filter(Store==1)%>%count(quarter)%>% 
  knitr::kable(caption="Week numbers of Q2 and Q3")%>%
  column_spec(1, width = "3cm")%>%column_spec(2, width = "3cm")%>%
  kable_styling(latex_options = c("striped","hold_position"))

#  compute quarterly sales for Q 2 & 3 
q2_3_sales<-q2_3_sales_all%>% group_by(Store,quarter)%>%
  summarize(quarter_sales=sum(Weekly_Sales))%>%
  mutate(Store=as.factor(Store),quarter=as.factor(quarter))

# Q 2 & 3 2012 quarterly sales plot
q2_3_sales%>%group_by(quarter)%>%ggplot(aes(Store,quarter_sales,fill=quarter))+
  geom_col(position = "dodge")+
  scale_fill_manual(values = alpha(c("blue", "red"), .9))+
  labs(title = "Plot of store sales, quater 2&3, 2012", y = "sales", 
       x = "store number", caption="Walmart Stores Sales")


# Q3 growth compute & plot

# Q3 absolute growth in dollars
q3_abs_grow<-q2_3_sales%>%group_by(Store)%>%
  summarize(quarter_grow=(quarter_sales-lag(quarter_sales)))%>%
  filter(!is.na(quarter_grow))

# Q3 relative growth in percentage
q3_grow<-q2_3_sales%>%filter(quarter==2)%>%left_join(q3_abs_grow)%>%
  mutate(percent=quarter_grow/quarter_sales*100,Store=as.numeric(Store))

# Q3 abs growth plot 
q3_grow%>%ggplot(aes(x=Store)) +
  geom_line(aes(y=quarter_grow), size=1, color="red") +geom_point(aes(y=quarter_grow))+ 
  scale_color_discrete(name="Growth",labels=c("Absolute value","Percentage"))+
  geom_hline(yintercept=0,size=1., color="green",linetype="dashed")+ 
  labs(title = "Plot of store sales growth, quater 3, 2012", x="store number", 
       y="sales growth ($)", caption="Walmart Stores Sales")

# Q3 relative growth plot 
q3_grow%>%ggplot(aes(x=Store)) +
  geom_line(aes(y=percent), size=1, color="blue") +geom_point(aes(y=percent))+ 
  geom_hline(yintercept=0,size=1., color="green",linetype="dashed")+ 
  labs(title = "Plot of store sales growth, quater 3, 2012", x="store number",
       y="sales growth (percentage)", caption="Walmart Stores Sales")

# table of good performance stores 
good_quart_store<-q3_grow%>%subset(quarter_grow>0)%>%
  select(Store,quarter_grow, percent)%>%arrange(desc(percent))
good_quart_store%>% knitr::kable(caption = "Stores with good growth")%>%
  column_spec(1, width = "3cm")%>%column_spec(2, width = "3cm")%>%
  column_spec(3, width = "3cm")%>%
  kable_styling(latex_options = c("striped","hold_position"))




### Task 4: Holidays with higher sales than the mean sales in non-holidays for all stores


# holiday store sale 
holiday_sale<-ndat%>%filter(Holiday_Flag==1)

# holiday store mean sale for individual holiday 
holiday_mean<-ndat%>%filter(Holiday_Flag==1)%>%group_by(Day_class)%>%
  summarize(mean_sale=mean(Weekly_Sales ))
holiday_mean

# non-holiday store sales
non_holiday_sale<-ndat%>%filter(Holiday_Flag==0)%>%group_by(Store)
# non-holiday mean store sales
non_holiday_mean<-ndat%>%filter(Holiday_Flag==0)%>%ungroup()%>%
  summarize(nonholiday_mean_sale=mean(Weekly_Sales ))
non_holiday_mean

# store sales each holiday compared to non-holiday sales, plot
holiday_sale%>%group_by(Day_class)%>%mutate(mean_sale=mean(Weekly_Sales))%>%
  ggplot(aes(Day_class,Weekly_Sales ))+geom_boxplot()+
  geom_line(aes(Day_class,mean_sale,group=1),color="blue")+
  geom_point(aes(Day_class,mean_sale,group=1),color="blue")+
  geom_text(aes(4,1471273,label="holiday mean", vjust=-0.5),color="blue")+
  geom_hline(yintercept=1041256,size=1., color="red",linetype="dashed")+ 
  geom_text(aes(4,1041256,label="non-holiday mean", vjust=-0.5),color="red")+
  labs(title = "Plot of store holiday sales", x="store number", 
       y="weekly sales($)", caption="Walmart Stores Sales")

# non-holiday sales for each store, plot
non_holiday_sale%>%mutate(Store=as.factor(Store))%>%ggplot(aes(Store,Weekly_Sales ))+
  geom_boxplot()+
  labs(title = "Plot of store non-holiday sales", x="store number", y="sales($)", 
       caption="Walmart Stores Sales")




### Task 5: Monthly and semester view of sales in units and insights 


# create new variables: month, year, quarter
ndat1<-ndat%>%mutate(month=month(Date),year=year(Date))

#set up quarters
ndat1<-ndat1%>%mutate(quarter=1)
# quarter 2
index<-ndat1$month>=4 & ndat1$month<=6
ndat1$quarter[index]<-2
# quarter 3
index<-ndat1$month>=7 & ndat1$month<=9
ndat1$quarter[index]<-3
# quarter 4
index<-ndat1$month>=10 & ndat1$month<=12
ndat1$quarter[index]<-4  

# make month, year, quarter factors
ndat1<-ndat1%>%mutate(month=as.factor(month),year=as.factor(year),quarter=as.factor(quarter))

# weeks per month plot
ndat1%>%filter(Store==1)%>%ggplot(aes(month,fill=year))+geom_bar(position = "dodge")+
  labs(title = "Plot of count of weeks per month", x="month", y="week numbers", caption="Walmart Stores Sales")

# weeks per quarter plot
ndat1%>%filter(Store==1)%>%ggplot(aes(quarter,fill=year))+geom_bar(position = "dodge")+
  labs(title = "Plot of count of weeks per quarter", x="quarter", y="week numbers", 
       caption="Walmart Stores Sales")
       

# monthly sale box plot
ndat1%>%group_by(year,month)%>%
  ggplot(aes(month,Weekly_Sales,color=year))+geom_boxplot()+
  labs(title = "Plot of monthly sales", x="month", y="sale($)", caption="Walmart Stores Sales")

# monthly mean sale bar plot
ndat1%>%group_by(year,month)%>%summarise(mean_sale_month=mean(Weekly_Sales))%>%
  ggplot(aes(month,mean_sale_month,fill=year))+geom_col(position = "dodge")+
  labs(title = "Plot of monthly mean sales", x="month", y="sale($)", caption="Walmart Stores Sales")

# quarterly sale box plot
ndat1%>%group_by(year,quarter)%>%
  ggplot(aes(quarter,Weekly_Sales,color=year))+geom_boxplot()+
  labs(title = "Plot of quaterly sales", x="month", y="sale($)", caption="Walmart Stores Sales")

# quarter mean sale bar plot
ndat1%>%group_by(year,quarter)%>%summarise(mean_sale_quarter=mean(Weekly_Sales))%>%
  ggplot(aes(quarter,mean_sale_quarter,fill=year))+geom_col(position = "dodge")+
  labs(title = "Plot of quaterly mean sales", x="month", y="sale($)", caption="Walmart Stores Sales")




######################## Modeling Data Analysis #######################

# Store 1 monthly sale box plot
ndat1%>%group_by(year,month)%>%filter(Store==1)%>%
  ggplot(aes(month,Weekly_Sales,color=year))+geom_boxplot()+
  labs(title = "Plot of Store 1 monthly sales", x="month", y="sale($)", caption="Walmart Stores Sales")

# Store 1 quarterly sale box plot
ndat1%>%group_by(year,quarter)%>%filter(Store==1)%>%
  ggplot(aes(quarter,Weekly_Sales,color=year))+geom_boxplot()+
  labs(title = "Plot of  Store 1 quaterly sales", x="month", y="sale($)", caption="Walmart Stores Sales")


# quarterly CPI box plot
ndat1%>%group_by(year,quarter)%>%filter(Store==1)%>%
  ggplot(aes(quarter,CPI,color=year))+geom_boxplot()+
  labs(title = "Plot of quaterly CPI", x="month", y="CPI", caption="Walmart Stores Sales")

# quarterly Unemployment box plot
ndat1%>%group_by(year,quarter)%>%filter(Store==1)%>%
  ggplot(aes(quarter,Unemployment,color=year))+geom_boxplot()+
  labs(title = "Plot of quaterly Unemployment", x="month", y="Unemployment", 
       caption="Walmart Stores Sales")

# quarterly Fuel_Price box plot
ndat1%>%group_by(year,quarter)%>%filter(Store==1)%>%
  ggplot(aes(quarter,Fuel_Price,color=year))+geom_boxplot()+
  labs(title = "Plot of quaterly Fuel_Price", x="month", y="Fuel_Price", 
       caption="Walmart Stores Sales")

######################## Data Analysis 
# create data only for data analysis
ndat2<-ndat1%>%filter(Store==1)%>%select(Weekly_Sales,Fuel_Price,CPI,Unemployment) 

# partition
set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(ndat2$Weekly_Sales, times = 1, p = 0.5, list = FALSE)
test <- ndat2[test_index,]
train <- ndat2[-test_index,]
list(n_train=nrow(train), n_test=nrow(test))

#### Model _1_: mu only

# model 1: mu only
mu <- mean(train$Weekly_Sales)
y_hat <- mu
rmse_1 <- RMSE(y_hat,test$Weekly_Sales)
rmse_1
rmse_results <- data_frame(method = "mu Only",model=1, RMSE = rmse_1)
rmse_results%>% knitr::kable()%>%kable_styling(latex_options = c("striped","hold_position"))

# data mismatch
plot(test$Weekly_Sales,col="red")
abline(h=y_hat,col="blue",lwd=2)
legend(1, 2300000, legend=c("Input", "Predicted"),
       col=c("red", "blue"), lty=1:2, cex=0.8)

#### Model _2_: mu + CPI  
# model 2: mu + CPI 
model="lm"
fit<-train( Weekly_Sales~CPI, method = model, data = train)
y_hat<-predict(fit, test)
rmse_2 <- RMSE(y_hat, test$Weekly_Sales)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "mu + CPI",model=2,
                                     RMSE = rmse_2))
rmse_results%>% knitr::kable()%>%kable_styling(latex_options = c("striped","hold_position"))

# data mismatch
plot(test$Weekly_Sales,col="red")
lines(y_hat,col="blue",lwd=2)
points(y_hat,col="blue",cex = .5)
legend(1, 2300000, legend=c("Input", "Predicted"),
       col=c("red", "blue"), lty=1:2, cex=0.8)

#### Model _3_: mu + CPI + Unemployment 
## model 3: mu + CPI + Unemployment 
model="lm"
fit<-train( Weekly_Sales~CPI+Unemployment, method = model, data = train)
y_hat<-predict(fit, test)
rmse_3 <- RMSE(y_hat, test$Weekly_Sales)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "mu + CPI + Unemployment",model=3,
                                     RMSE = rmse_3))
rmse_results%>% knitr::kable()%>%kable_styling(latex_options = c("striped","hold_position"))

# data mismatch
plot(test$Weekly_Sales,col="red")
lines(y_hat,col="blue",lwd=2)
points(y_hat,col="blue",cex = .5)
legend(1, 2300000, legend=c("Input", "Predicted"),
       col=c("red", "blue"), lty=1:2, cex=0.8)


#### Model _4_: mu + CPI + Unemployment + Fuel_Price

# model 4: mu + CPI + Unemployment + Fuel_Price
model="lm"
fit<-train( Weekly_Sales~CPI+Unemployment+Fuel_Price, method = model, data = train)
y_hat<-predict(fit, test)
rmse_4 <- RMSE(y_hat, test$Weekly_Sales)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "mu + CPI + Unemployment + Fuel_Price",model=4,
                                     RMSE = rmse_4))
rmse_results%>% knitr::kable()%>%kable_styling(latex_options = c("striped","hold_position"),
                                               position = "center")
varImp(fit)
# data mismatch
plot(test$Weekly_Sales,col="red")
lines(y_hat,col="blue",lwd=2)
points(y_hat,col="blue",cex = .5)
legend(1, 2300000, legend=c("Input", "Predicted"),
       col=c("red", "blue"), lty=1:2, cex=0.8)

#### Model _5_: K-Nearest Neighbors

#model 5: knn:  mu + CPI + Unemployment + Fuel_Price
set.seed(2, sample.kind = "Rounding")
model="knn"
train_control <- trainControl(method="cv", number=5, p=0.9)
fit<-train( Weekly_Sales~CPI+Unemployment+Fuel_Price, method = model, data = train,
            tuneGrid = data.frame(k = seq(1, 71, 2)),trControl=train_control)

y_hat<-predict(fit, test)
rmse_5 <- RMSE(y_hat, test$Weekly_Sales)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "knn: mu + CPI + Unemployment + Fuel_Price",model=5,
                                     RMSE = rmse_5))
rmse_results%>% knitr::kable()%>%kable_styling(latex_options = c("striped","hold_position"),
                                               position = "center")

#minimum k
fit$results$k[which.min(fit$results$RMSE)]
ggplot(fit)

plot(test$Weekly_Sales,col="red")
lines(y_hat,col="blue",lwd=2)
points(y_hat,col="blue",cex = .5)
legend(1, 2300000, legend=c("Input", "Predicted"),
       col=c("red", "blue"), lty=1:2, cex=0.8)


#### Model _6_: Random Forests

set.seed(3, sample.kind = "Rounding")
model="rf"
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
fit<-train( Weekly_Sales~CPI+Unemployment+Fuel_Price, method = model, data = train, tuneGrid = data.frame(mtry = seq(1,70,10)),
            ntree=100, trControl=control)
y_hat<-predict(fit, test)
rmse_6 <- RMSE(y_hat, test$Weekly_Sales)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "rf: mu + CPI + Unemployment + Fuel_Price",model=6,
                                     RMSE = rmse_6))
rmse_results%>% knitr::kable()%>%
  kable_styling(latex_options = c("striped","hold_position"),position = "center")

#minimum mtry
fit$results$mtry[which.min(fit$results$RMSE)]
plot(fit)

varImp(fit)

# data mismatch
plot(test$Weekly_Sales,col="red")
lines(y_hat,col="blue",lwd=2)
points(y_hat,col="blue",cex = .5)
legend(1, 2300000, legend=c("Input", "Predicted"),
       col=c("red", "blue"), lty=1:2, cex=0.8)


## prepare for final plot
rmse_results$method <- reorder(rmse_results$method, rmse_results$model)

############### final results plot
rmse_results %>%
  ggplot(aes(model, RMSE, fill = method)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  coord_cartesian(ylim = c(180000, 190000)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Walmart weekly store sales moldels' result" , x = "model", caption = "")

