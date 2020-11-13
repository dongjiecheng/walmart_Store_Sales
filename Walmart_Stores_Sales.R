###
library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(magrittr) 
library(png) 
library(lubridate) 
library(wesanderson)
library(viridis)
# 5 significant digits
options(digits = 5)

######################## load and partition #######################
# Original Walmart Stores Sales  dataset location:
# https://www.kaggle.com/vik2012kvs/walmart-dataretail-analysis/

# New location on GitHub
url<- "https://raw.githubusercontent.com/dongjiecheng/walmart_Store_Sales/main/Walmart_Store_sales.csv"

#read in data
dat<-read_csv(url)

#glimpse
glimpse(dat)

#class of variables
lapply(dat, class)

#check NA
sapply(dat, function(x)
  sum(is.na(x)))

# # convert date to numeric
ndat<-dat%>%mutate(Date=dmy(dat$Date),nDate=as.numeric(Date))%>%mutate(
  Holiday_Flag=factor(Holiday_Flag),
  Day_class=ifelse(Holiday_Flag==1,"Holiday","Normalday")
)

# replace the holidays' names 

index<-as.character(ndat$Date)%in%c("2010-02-12","2011-02-11","2012-02-10","2013-02-08")
ndat$Day_class[index]<-"Super_Bowl"
index<-as.character(ndat$Date)%in%c("2010-09-10","2011-09-09","2012-09-07","2013-09-06")
ndat$Day_class[index]<-"Labor_Day"
index<-as.character(ndat$Date)%in%c("2010-11-26","2011-11-25","2012-11-23","2013-11-29")
ndat$Day_class[index]<-"Thanksgiving"
index<-as.character(ndat$Date)%in%c("2010-12-31","2011-12-30","2012-12-28","2013-12-27")
ndat$Day_class[index]<-"Christmas"

#################### Data QC #############
#check NA
sum(is.na(ndat$Date))

# Check days
ndat%>%group_by(Store)%>%summarise(sum=n())%>%.$sum

# check date distribution
ndat%>%group_by(Date)%>%ggplot(aes(Date,Store,color=Day_class))+geom_point(size=0.01)

# map view
mid<-mean(dat$Weekly_Sales)
dat%>%ggplot(aes(Store,Date,color=Weekly_Sales))+
  geom_point()+scale_color_gradient2(midpoint=mid, low="blue", mid="white",
                                     high="red", space ="Lab" )

# sales histogram plots
ndat%>%group_by(Weekly_Sales)%>%
  ggplot(aes(Weekly_Sales))+geom_histogram(fill="blue",color="red")+scale_x_log10()+  
  scale_fill_brewer(palette = "Spectral")+theme_dark() +guides(color = "none")+
  labs(title = "Histogram of Weekly_Sales", y = "count", x = "rating", caption="Walmart Stores Sales")

# store sales mean barplot
ndat%>%group_by(Store)%>%summarize(sale_mean=mean(Weekly_Sales),std=sd(Weekly_Sales),n=n())%>%
  mutate(low=sale_mean-std/sqrt(n),high=sale_mean+std/sqrt(n))%>%
  ggplot(aes(Store,sale_mean))+ scale_y_log10()+ 
  geom_point(color="red")+
  geom_errorbar(aes(ymin=low, ymax=high), color="blue", width=.9, position=position_dodge(.9))+
  labs(title = "Bar plot of store sales", y = "store sales mean", x = "store number", caption="Walmart Stores Sales")

################### store sales mean bar+bar plot
ndat%>%group_by(Store)%>%mutate(sale_mean=mean(Weekly_Sales),std=sd(Weekly_Sales),n=n())%>%
  mutate(low=sale_mean-std/sqrt(n),high=sale_mean+std/sqrt(n))%>%
  ggplot(aes(Store,sale_mean))+
  geom_bar(stat="identity", color="blue", fill="white",
           position=position_dodge())+
  geom_errorbar(aes(ymin=low, ymax=high), width=.9, position=position_dodge(.9))+
  labs(title = "Bar plot of store sales", y = "store sales mean", x = "store number", caption="Walmart Stores Sales")

# store total sales 
ndat%>%group_by(Store)%>%summarize(whole_sale=sum(Weekly_Sales))%>%
  ggplot() +
  geom_line(aes(Store,whole_sale,color="blue"))+geom_point(aes(Store,whole_sale,color="blue"))+
  labs(title = "Plot of total store sales", y = "sales", x = "store number", caption="Walmart Stores Sales")


# store sales standard deviation and standard errors line plots
ndat%>%group_by(Store)%>%summarize(sale_mean=mean(Weekly_Sales),std=sd(Weekly_Sales),n=n())%>%
  mutate(ste=std/sqrt(n))%>%
  ggplot() +
  geom_line(aes(Store,std,color="blue"))+geom_point(aes(Store,std,color="blue"))+
  geom_line(aes(Store,ste,color="red"))+geom_point(aes(Store,ste,color="red"))+
  scale_color_discrete(name="Errors",labels=c("Standard deviation","Standard error"))+
  labs(title = "Plot of store sales standard deviatiob and standard error", y = "error", x = "store number", caption="Walmart Stores Sales")

# store with maximum total sales
ndat%>%group_by(Store)%>%summarize(whole_sale=sum(Weekly_Sales),sale_mean=mean(Weekly_Sales))%>%
  summarize(max_whole_sale=max(whole_sale),store_max_whole_sale=Store[which.max(whole_sale)])
  
# store with maximum mean sales  
ndat%>%group_by(Store)%>%summarize(sale_mean=mean(Weekly_Sales),std=sd(Weekly_Sales))%>%
  summarize(max_mean=(max(sale_mean)),store_max_mean=Store[which.max(sale_mean)])

# store with maximum standard deviation
ndat%>%group_by(Store)%>%summarize(sale_mean=mean(Weekly_Sales),std=sd(Weekly_Sales))%>%
  summarize(max_standard_deviation=(max(std)),store_max_standard_deviation=which.max(std),
            store_mean=sale_mean[which.max(std)] )

# Q3 2012 growth
# all Q 2&3 2012 sales
q2_3_sales_all<-ndat%>%filter(Date>"2012-03-31"&Date<"2012-10-01")%>%mutate(quarter=ifelse(Date>"2012-06-30",3,2))
# count weeks
q2_3_sales_all%>%filter(Store==1)%>%count(quarter)

# select Q 2&3 sales
q2_3_sales<-q2_3_sales_all%>% group_by(Store,quarter)%>%summarize(quarter_sales=sum(Weekly_Sales))%>%
  mutate(Store=as.factor(Store),quarter=as.factor(quarter))
# Q 2&3 2012 sales plot
q2_3_sales%>%arrange(quarter)%>%ggplot(aes(Store,quarter_sales,fill=quarter))+geom_col(position = "dodge")+
 scale_fill_manual(values = alpha(c("blue", "red"), .9))+
labs(title = "Plot of store sales, quater 2&3, 2012", y = "sales", x = "store number", caption="Walmart Stores Sales")

# Q3 growth compute & plot

# Q3 absolute growth
q3_abs_grow<-q2_3_sales%>%group_by(Store)%>%
  summarize(quarter_grow=(quarter_sales-lag(quarter_sales)))%>%
  filter(!is.na(quarter_grow))

# Q3 abs&relative growth
q3_grow<-q2_3_sales%>%filter(quarter==2)%>%left_join(q3_abs_grow)%>%
  mutate(percent=quarter_grow/quarter_sales*100,Store=as.numeric(Store))

### best performance 
 
best_quart_dollar<-q3_grow%>%ungroup()%>%summarize(best_dollar=max(quarter_grow), best_dollar_store=Store[which.max(quarter_grow)],
                                       best_percent=max(percent),best_percent_store=Store[which.max(percent)])

best_quart_dollar

# Q3 abs growth plot 
q3_grow%>%ggplot(aes(x=Store)) +
  geom_line(aes(y=quarter_grow), size=1, color="red") +geom_point(aes(y=quarter_grow))+ 
  scale_color_discrete(name="Growth",labels=c("Absolute value","Percentage"))+
  labs(title = "Plot of store sales growth, quater 3, 2012", x="store number", y="sales growth ($)", caption="Walmart Stores Sales")

# Q3 relative growth plot 
q3_grow%>%ggplot(aes(x=Store)) +
  geom_line(aes(y=percent), size=1, color="blue") +geom_point(aes(y=percent))+ 
  labs(title = "Plot of store sales growth, quater 3, 2012", x="store number", y="sales growth (percentage)", caption="Walmart Stores Sales")


##### Q3 abs&relative growth plot together
scal=100000

q3_grow%>%ggplot(aes(x=Store)) +
  geom_line(aes(y=quarter_grow), size=1, color="red") +geom_point(aes(y=quarter_grow))+ 
  geom_line(aes(y=percent*scal), size=1, color="blue") +geom_point(aes(y=percent*scal))+ 
  scale_y_continuous(
    # Features of the first axis
    name = "sales growth ($)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*scal, name="sales growth percentage")
  ) + 
  scale_color_discrete(name="Growth",labels=c("Absolute value","Percentage"))+
  labs(title = "Plot of store sales growth, quater 3, 2012", caption="Walmart Stores Sales")

############## Task 4  Holidays with higher sales than the mean sales in

# holiday store sale 
holiday_sale<-ndat%>%filter(Holiday_Flag==1)

# holiday store mean sale per individual holiday 
holiday_mean<-ndat%>%filter(Holiday_Flag==1)%>%group_by(Day_class)%>%summarize(mean_sale=mean(Weekly_Sales ))
holiday_mean

# non-holiday store sales
non_holiday_sale<-ndat%>%filter(Holiday_Flag==0)%>%group_by(Store)

# non-holiday mean store sales
non_holiday_mean<-ndat%>%filter(Holiday_Flag==0)%>%ungroup()%>%summarize(nonholiday_mean_sale=mean(Weekly_Sales ))
non_holiday_mean


# store sales each holiday compared to non-holiday sales, plot
holiday_sale%>%group_by(Day_class)%>%mutate(mean_sale=mean(Weekly_Sales))%>%
  ggplot(aes(Day_class,Weekly_Sales ))+geom_boxplot()+
  geom_line(aes(Day_class,mean_sale,group=1),color="blue")+
  geom_point(aes(Day_class,mean_sale,group=1),color="blue")+
  geom_text(aes(4,1471273,label="holiday mean", vjust=-0.5),color="blue")+
  geom_hline(yintercept=1041256,size=1., color="red",linetype="dashed")+ 
  geom_text(aes(4,1041256,label="non-holiday mean", vjust=-0.5),color="red")+
     labs(title = "Plot of store holiday sales", x="store number", y="weekly sales($)", caption="Walmart Stores Sales")
 

# non-holiday sales for each store, plot
non_holiday_sale%>%mutate(Store=as.factor(Store))%>%ggplot(aes(Store,Weekly_Sales ))+geom_boxplot()+
labs(title = "Plot of store non-holiday sales", x="store number", y="sales($)", caption="Walmart Stores Sales")

############## Task 5  Holidays with higher sales than the mean sales in


# create flags, month, year, day, quarter
ndat1<-ndat%>%mutate(month=month(Date),year=year(Date))

# delete nDate
ndat1<-ndat1[-9]

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

ndat1%>%ggplot(aes(month,fill=year))+geom_bar(position = "dodge")+theme_dark()+
labs(title = "Plot of count of weeks per month", x="store number", y="week numbers", caption="Walmart Stores Sales")

ndat1%>%ggplot(aes(quarter,fill=year))+geom_bar(position = "dodge")+theme_dark()

ndat1%>%group_by(year,month)%>%ggplot(aes(month,Weekly_Sales,color=year))+geom_boxplot()




# Single store sales

mid=median(dat$Store)
holiday_sale<-ndat%>%filter(Holiday_Flag==1)%>%group_by(Store)
holiday_sale%>%filter(Store==1)%>%ggplot(aes(Day_class,Weekly_Sales ))+geom_boxplot()


dat%>%filter(Store==1)%>%ggplot(aes(Date,Weekly_Sales))+geom_line()
dat%>%group_by(Store)%>%filter(Store==c(seq(1:10)))%>%ggplot(aes(ndate,Weekly_Sales,color=Store))+
  geom_line()+scale_color_gradient2(midpoint=mid, low="red", mid="green",
                      high="blue", space ="Lab" )

p_store<-sample(seq(1:45),5, replace = FALSE)
d<-dat%>%group_by(Store)%>%filter(Store==p_store)
d%>%ggplot(aes(ndate,Weekly_Sales,color=as.factor(Store),label=Store))+
  geom_line()+scale_color_viridis( option = "D",discrete = TRUE)+geom_text()

dat%>%ggplot(aes(ndate,Weekly_Sales,color=as.factor(Store)))+
  geom_line()+scale_color_viridis( option = "D",discrete = TRUE)
###scale_fill_viridis()+ theme_bw()

mean_sale<-mean(dat$Weekly_Sales)
dat%>%filter(Store==1)%>%ggplot(aes(x=Date,y=Holiday_Flag,fill=Weekly_Sales-mean_sale))+
  geom_col()+scale_color_viridis( option = "B")

# temperature vs sale 
mid=median(dat$Store)
dat%>%filter(Store==1)%>%ggplot(aes(Date,Weekly_Sales))+geom_line()
dat%>%group_by(Store)%>%filter(Store==c(seq(1:10)))%>%ggplot(aes(ndate,Weekly_Sales,color=Store))+
  geom_line()+scale_color_gradient2(midpoint=mid, low="red", mid="green",
                                                                                                                                    high="blue", space ="Lab" )

# 
hist(dat$CPI)
hist(dat$Unemployment)
hist(dat$ndate)
hist(dat$Holiday_Flag)
hist(dat$Temperature)
hist(dat$Fuel_Price)

# split file
set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
ndat_store1<-ndat%>%filter(Store==1)
test_index <- createDataPartition(ndat_store1$Weekly_Sales, times = 1, p = 0.5, list = FALSE)
test <- ndat_store1[test_index,]
train <- ndat_store1[-test_index,]


models <- c("glm", "svmLinear", "knn", "gamLoess", "multinom",  "rf" )

model="rf"
fit<-train( Weekly_Sales~CPI+Unemployment+Fuel_Price   , method = model, data = train)
y_hat<-predict(fit, test)
rmse_glm <- RMSE(y_hat, test$Weekly_Sales)
rmse_glm


