#Data Analysis Using Uber Dataset
#Data Source -> https://www.kaggle.com/fivethirtyeight/uber-pickups-in-new-york-city

#Importing all the files and required library
library(ggplot2) #visualization matplotlib in case of python
library(ggthemes) #add on with ggplot
library(dplyr) #data manipulation pandas in case of python
library(lubridate) #date time
library(scales) #graphical scaling
library(tidyr) #tidy data
library(DT) #table formatted data
library(viridis) # visualization gradient color palet

#reading the data (6 months of data)
apr_data<-read.csv("uber-raw-data-apr14.csv")
may_data<-read.csv("uber-raw-data-may14.csv")
jun_data<-read.csv("uber-raw-data-jun14.csv")
jul_data<-read.csv("uber-raw-data-jul14.csv")
aug_data<-read.csv("uber-raw-data-aug14.csv")
sep_data<-read.csv("uber-raw-data-sep14.csv")

#combining all data
data_2014<-rbind(apr_data,may_data,jun_data,jul_data,aug_data,sep_data)
data_2014$Date.Time<-as.factor(data_2014$Date.Time)
data_2014$Base<-as.factor(data_2014$Base)


#visualize the data
head(data_2014)
#structure 
str(data_2014)
#summary_statistics
summary(data_2014)

#5 bases are there and lat lon taken from usa operation

#analysis
data_2014$Date.Time<-as.POSIXct(data_2014$Date.Time,format="%m/%d/%Y %H:%M:%S")


#summary statistics
summary(data_2014)

#Extracting Time from date time
data_2014$Time<-format(as.POSIXct(data_2014$Date.Time,format="%m/%d/%Y %H:%M:%S"),format ="%H:%M:%S" )

data_2014$Date.Time<-ymd_hms(data_2014$Date.Time) #formatting
data_2014$day<-format(day(data_2014$Date.Time)) #extracting day
data_2014$month<-format(month(data_2014$Date.Time,label = TRUE)) #extracting month
data_2014$year<-format(year(data_2014$Date.Time)) #extracting year
data_2014$dayofweek<-format(wday(data_2014$Date.Time,label = TRUE)) #extracting day of week

#hour minute second extraction
data_2014$hour<- factor(hour(hms(data_2014$Time)))
data_2014$minute<- factor(minute(hms(data_2014$Time)))
data_2014$second<- factor(second(hms(data_2014$Time)))

#full data
head(data_2014)


#Visualization

#plotting trip by hours in a day
hour_data<- data_2014 %>%
  group_by(hour) %>% 
  summarise(Total=n()) #grouping the data wrt hour and count

#grouping using sqldf package
library(sqldf)
sqldf("select hour,count(*) from data_2014 group by hour order by hour desc")

#see in tabular form
datatable(hour_data)

#visualize the data
ggplot(hour_data,aes(hour,Total))+
  geom_bar(stat = "identity", fill="black",color="blue")+
  ggtitle("Trips by hour")+
  theme(legend.position = "none")+
  scale_y_continuous(labels = comma)

#Finding 1 - > most of the operations happend during 15 to 21 aka 3pm to 9pm

month_hour_data<-data_2014 %>% 
  group_by(month,hour) %>% 
  summarise(Total=n())

#see in tabular form
datatable(month_hour_data)

#visualize the data
ggplot(month_hour_data,aes(hour,Total,fill=month))+
  geom_bar(stat = "identity")+
  ggtitle("Trips by hour and month")+
  scale_y_continuous(labels = comma)
# September has more riders than ever

sept_hour<-data_2014 %>% 
  group_by(hour,month) %>% 
  filter(month=='Sep') %>% 
  summarise(Total=n())

datatable(sept_hour)

#visualize the data
ggplot(sept_hour,aes(hour,Total,fill=hour))+
  geom_bar(stat = "identity")+
  ggtitle("Trips by hour and month for September")+
  scale_y_continuous(labels = comma)

#plot data by day
day_data<-data_2014 %>% 
  group_by(day) %>% 
  summarise(Total=n())

datatable(day_data)

#visualize the data
ggplot(day_data,aes(day,Total))+
  geom_bar(stat = "identity", fill="blue",color="black")+
  ggtitle("Trips by day")+
  theme(legend.position = "none")+
  scale_y_continuous(labels = comma)



# month day grouping

month_day_data<-data_2014 %>% 
  group_by(month,day) %>% 
  summarise(Total=n())

#see in tabular form
datatable(month_day_data)

#visualize the data
ggplot(month_day_data,aes(day,Total,fill=month))+
  geom_bar(stat = "identity")+
  ggtitle("Trips by hour and month")+
  scale_y_continuous(labels = comma)


#monthly trend

month_data<-data_2014 %>% 
  group_by(month) %>% 
  summarise(Total=n())

datatable(month_data)

#analysis of bases
ggplot(data_2014,aes(Base))+
  geom_bar(fill="darkred")+
  ggtitle("Trips by base")+
  scale_y_continuous(labels = comma)


ggplot(data_2014,aes(Base,fill=month))+
  geom_bar(position ="dodge")+
  ggtitle("Trips by base and month")+
  scale_y_continuous(labels = comma)



min_lat<-40.5774
max_lat<-40.9176
min_long<- 74.15
max_long<-73.7004

ggplot(data_2014,aes(x=Lon,y=Lat))+
  geom_point(size=1)+
  scale_x_continuous(limits = c(min_long,max_long))+
  scale_y_continuous(limits = c(min_lat,max_lat))+
  theme_map()+
  ggtitle("NYC Map Based on UBER Rides during 2014(Apr-Sep) by Base")


