setwd("C:/Users/Santosh/Desktop/Datasets-20181022/flight-delays")
# install.packages("data.table")
# install.packages("DT")
# install.packages("dplyr")
# install.packages("mongolite")
# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("maps")
# install.packages("leaflet")
# install.packages("cron")
# install.packages("lubridate")
# install.packages("doBy")
# install.packages("ggfortify")
# install.packages("digest")
library("DT")
library("data.table") 
library("mongolite")
library("lubridate")
library("dplyr")
library("ggplot2")
library("gridExtra")
library("chron")
library("ggthemes") 
library("ggfortify")
library("zoo")
library("digest")
library("doBy")


#################################################STEP 1#######################################################


#STEP 1.1
# Read the CSV first to clean the data before we upload the data to MongoDB
flights <- fread("flights.csv") 
airlines <- fread("airlines.csv") 
airports <- fread("airports.csv") 

unique(flights$ORIGIN_AIRPORT)
flights[grep('[0-9]+', flights$ORIGIN_AIRPORT),]
#DATA OF ORIGIN_AIRPORT & DEST_AIRPORT IS IN NUMERIC 
# FORMAT FOR WHOLE 10TH MONTH HENCE CANNOT BE JOINED WITH AIRPORTS DATA WHICH HAS 
#CHARACTER CODES ONLYIN UPCOMING STEPS
#HENCE ASSUMING THE PATTERN REMAINS SAME BETWEEN MONTHS 
#THE RANDOM SAMPLE IS TAKEN FROM DIFFRENT MONTHS
#AND MERGED AS THE 10th MONTH DATA
#e.g
flights[flights$ORIGIN_AIRPORT == "12511",]

#Taking the count of rows of 10th month
missing_data_count <- nrow(flights[grep('[0-9]+', flights$ORIGIN_AIRPORT),])

#Taking data except 10th month into new dataframe
flights_without_10th_mon <- flights[!grepl('[0-9]+', flights$ORIGIN_AIRPORT),]
#e.g validate missing 10th column
unique(flights_without_10th_mon$MONTH)


missing_data <- flights_without_10th_mon[sample(nrow(flights_without_10th_mon),missing_data_count,replace=FALSE),]
unique(missing_data$MONTH)
missing_data$MONTH <- 10

flights <- rbind(flights_without_10th_mon,missing_data)
flights <- flights[order(flights$YEAR,flights$MONTH,flights$DAY)]
rm(missing_data_count,flights_without_10th_mon,missing_data)
#STEP 1.1 - END

#STEP 1.2
#Missing Values Analysis to check and handle the missing data from the source dataset
Col_class_name = sapply(flights, class)
Column_missing_count = colSums(is.na(flights))
Column_missing_percent = colSums(is.na(flights)) / nrow(flights) * 100 
missing_values <- cbind(Col_class_name,Column_missing_count,round(Column_missing_percent,2))
colnames(missing_values)[3] <- "Percent %"
datatable(missing_values)
rm(Column_missing_percent,Column_missing_count,missing_values,Col_class_name)

Col_class_name = sapply(airlines, class)
Column_missing_count = colSums(is.na(airlines))
Column_missing_percent = colSums(is.na(airlines)) / nrow(airlines) * 100 
missing_values <- cbind(Col_class_name,Column_missing_count,round(Column_missing_percent,2))
colnames(missing_values)[3] <- "Percent %"
datatable(missing_values)
rm(Column_missing_percent,Column_missing_count,missing_values,Col_class_name)

Col_class_name = sapply(airports, class)
Column_missing_count = colSums(is.na(airports))
Column_missing_percent = colSums(is.na(airports)) / nrow(airports) * 100 
missing_values <- cbind(Col_class_name,Column_missing_count,round(Column_missing_percent,2))
colnames(missing_values)[3] <- "Percent %"
datatable(missing_values)
rm(Column_missing_percent,Column_missing_count,missing_values,Col_class_name)
#STEP 1.2 - END


#STEP 1.3
#Date is separated in 3 diffrent columns, We merge all the columns into one column called DATE
DATE <- paste(flights$YEAR, flights$MONTH,flights$DAY, sep="-") %>% ymd() %>% as.Date()
flights$YEAR = NULL
flights$MONTH = NULL
flights$DAY = NULL
flights = cbind(DATE,flights)
rm(DATE)
#STEP 1.3 - END
#STEP 1.4
#Skipping the rows which have NA for these columns as the missing value analysis show only 4% of the 
#missing data of these columns collectively.
flights = flights[!is.na(flights$SCHEDULED_ARRIVAL), ]
flights = flights[!is.na(flights$SCHEDULED_DEPARTURE), ]
flights = flights[!is.na(flights$DEPARTURE_TIME), ]
flights = flights[!is.na(flights$ARRIVAL_TIME), ]
flights = flights[!is.na(flights$ELAPSED_TIME), ]
flights = flights[!is.na(flights$AIR_TIME), ]
airports = airports[!is.na(airports$LATITUDE ), ]
airports = airports[!is.na(airports$LONGITUDE ), ]
#STEP 1.4 - END

#STEP 1.5
#The 81% missing value for below columns is not an issue as it just means the flights were not 
#Delayed and they were on time. 
#For the analysis purpose we make These NA Colums as 0
flights$AIR_SYSTEM_DELAY[is.na(flights$AIR_SYSTEM_DELAY)] <- 0
flights$SECURITY_DELAY[is.na(flights$SECURITY_DELAY)] <- 0
flights$AIRLINE_DELAY[is.na(flights$AIRLINE_DELAY)] <- 0
flights$LATE_AIRCRAFT_DELAY[is.na(flights$LATE_AIRCRAFT_DELAY)] <- 0
flights$WEATHER_DELAY[is.na(flights$WEATHER_DELAY)] <- 0
#STEP 1.5- END


#STEP 1.6
#Change the DAY_OF_WEEK Column values to the name of the days rather than the numbers.
flights$DAY_OF_WEEK  <- ifelse(flights$DAY_OF_WEEK == 1, "Monday",
                        ifelse(flights$DAY_OF_WEEK == 2, "Tuesday",
                        ifelse(flights$DAY_OF_WEEK == 3, "Wednesday",
                        ifelse(flights$DAY_OF_WEEK == 4, "Thursday",
                        ifelse(flights$DAY_OF_WEEK == 5, "Friday",
                        ifelse(flights$DAY_OF_WEEK == 6, "Saturday",
                        ifelse(flights$DAY_OF_WEEK == 7, "Sunday",NA)))))))

#STEP 1.6 - END




#STEP 1.7
#Missing Values Analysis to Validate the cleaning of Data
Col_class_name = sapply(flights, class)
Column_missing_count = colSums(is.na(flights))
Column_missing_percent = colSums(is.na(flights)) / nrow(flights) * 100 
missing_values <- cbind(Col_class_name,Column_missing_count,round(Column_missing_percent,2))
colnames(missing_values)[3] <- "Percent %"
datatable(missing_values)
rm(Column_missing_percent,Column_missing_count,missing_values,Col_class_name)

Col_class_name = sapply(airlines, class)
Column_missing_count = colSums(is.na(airlines))
Column_missing_percent = colSums(is.na(airlines)) / nrow(airlines) * 100 
missing_values <- cbind(Col_class_name,Column_missing_count,round(Column_missing_percent,2))
colnames(missing_values)[3] <- "Percent %"
datatable(missing_values)
rm(Column_missing_percent,Column_missing_count,missing_values,Col_class_name)

Col_class_name = sapply(airports, class)
Column_missing_count = colSums(is.na(airports))
Column_missing_percent = colSums(is.na(airports)) / nrow(airports) * 100 
missing_values <- cbind(Col_class_name,Column_missing_count,round(Column_missing_percent,2))
colnames(missing_values)[3] <- "Percent %"
datatable(missing_values)
rm(Column_missing_percent,Column_missing_count,missing_values,Col_class_name)
#Missing Values Analysis -- END
#STEP 1.7 - END


#1.8 Removing Unnecessary Columns
flights$FLIGHT_NUMBER = NULL
flights$TAIL_NUMBER = NULL
flights$CANCELLATION_REASON = NULL
flights$DIVERTED = NULL
flights$CANCELLED = NULL
#STEP 1.8 - END





#################################################STEP 1 END#######################################################



#################################################STEP 2#######################################################

#DATABASE PREPARATION 
#Step 2.1 Uploading Airlines and Airports Data To MongoDB
airlines_db = mongo(collection = "Airlines", db = "Assignment3")
airlines_db$insert(airlines)

airports_db = mongo(collection = "Airports", db = "Assignment3")
airports_db$insert(airports)
rm(airlines_db,airports_db)
#Step 2.2 
#Connecting to MONGODB to Get the Airlines data & Airports Data 
airlines = mongo(collection = "Airlines", db = "Assignment3")
airports = mongo(collection = "Airports",db="Assignment3")
airlines_data = airlines$find('{}') 
airlines_data <- na.omit(airlines_data)
airports_data = airports$find('{}') 
airports_data <- na.omit(airports_data)
#STEP 2.2 - END

#STEP 2.3  JOINING THE DATASETS
colnames(airlines_data)[1] <- "AIRLINE_ID"
colnames(flights)[3] <- "AIRLINE_ID"
airline_flights <- full_join(flights,airlines_data,by = c("AIRLINE_ID"))


colnames(airports_data) <- c("ORIG_AIRPORTID","ORIG_AIRPORT_NAME","ORIG_AIRPORT_CITY","ORIG_STATE","ORIG_COUNTRY","ORIG_LATITUDE","ORIG_LONGITUDE")
colnames(airline_flights)[4] <- "ORIG_AIRPORTID"
airline_flights <- full_join(airline_flights,airports_data,by = c("ORIG_AIRPORTID"))
 

colnames(airports_data) <- c("DEST_AIRPORTID","DEST_AIRPORT_NAME","DEST_AIRPORT_CITY","DEST_STATE","DEST_COUNTRY","DEST_LATITUDE","DEST_LONGITUDE")
colnames(airline_flights)[5] <- "DEST_AIRPORTID"
airline_flights <- full_join(airline_flights,airports_data,by = c("DEST_AIRPORTID"))
rm(flights,airports,airlines,airports_data,airlines_data)

#STEP 2.3 - END

#2.4 Subsetting Data
#DUE TO DATA STORAGE ISSUE & HARDWARE LIMITATION THE 50% SAMPLE OF DATA IS TAKEN ASSUMING THE 
#UNDERLYING TRENDS REMAIN SAME
airline_flights = airline_flights[sample(nrow(airline_flights),(nrow(airline_flights)/2)),]


#Step 2.4 End
 
#STEP 2.5
#INSERTING THE CLEANED flights DATA TO MONGODB
flights = mongo(collection = "Flights_Subset", db = "Assignment3")
flights$insert(airline_flights)
rm(airline_flights)
#STEP 2.5 - END
#################################################STEP 2 END#######################################################
  

#################################################STEP 3#######################################################

#DATA EXPLORATION 
flights = mongo(collection = "Flights_Subset", db = "Assignment3")
#3.1 Basic Statistics Gathering
#Query on Mongo DB With Same Mongo Object
airline_avg_delay = flights$aggregate('[
                                      { 
                                      "$group" : {
                                      "_id" : {
                                      "AIRLINE" : "$AIRLINE"
                                      }, 
                                      "AVG(DEPARTURE_DELAY)" : {
                                      "$avg" : "$DEPARTURE_DELAY"
                                      }
                                      }
                                      }, 
                                      { 
                                      "$project" : {
                                      "_id" : 0, 
                                      "AIRLINE" : "$_id.AIRLINE", 
                                      "AVG(DEPARTURE_DELAY)" : "$AVG(DEPARTURE_DELAY)"
                                      }
                                      }
                                      ]')
airline_avg_delay = na.omit(airline_avg_delay)

airline_max_delay = flights$aggregate('[
                                      { 
                                      "$group" : {
                                      "_id" : {
                                      "AIRLINE" : "$AIRLINE"
                                      }, 
                                      "MAX(DEPARTURE_DELAY)" : {
                                      "$max" : "$DEPARTURE_DELAY"
                                      }
                                      }
                                      }, 
                                      { 
                                      "$project" : {
                                      "_id" : 0, 
                                      "AIRLINE" : "$_id.AIRLINE", 
                                      "MAX(DEPARTURE_DELAY)" : "$MAX(DEPARTURE_DELAY)"
                                      }
                                      }
                                      ]')
airline_max_delay = na.omit(airline_max_delay)
airline_min_delay = flights$aggregate('[
                                      { 
                                      "$group" : {
                                      "_id" : {
                                      "AIRLINE" : "$AIRLINE"
                                      }, 
                                      "MIN(DEPARTURE_DELAY)" : {
                                      "$min" : "$DEPARTURE_DELAY"
                                      }
                                      }
                                      }, 
                                      { 
                                      "$project" : {
                                      "_id" : 0, 
                                      "AIRLINE" : "$_id.AIRLINE", 
                                      "MIN(DEPARTURE_DELAY)" : "$MIN(DEPARTURE_DELAY)"
                                      }
                                      }
                                      ]')
airline_min_delay = na.omit(airline_min_delay)
airline_avg_delay$`AVG(DEPARTURE_DELAY)` <- round(airline_avg_delay$`AVG(DEPARTURE_DELAY)`,2)
colnames (airline_avg_delay)[2] <- "Average Delay"
colnames (airline_max_delay)[2] <- "MAX Delay"
colnames (airline_min_delay)[2] <-  "MIN Delay"
flights_general_stats <- cbind(airline_avg_delay,airline_max_delay[2],airline_min_delay[2])
datatable(flights_general_stats)
rm(airline_avg_delay,airline_max_delay,airline_min_delay,flights_general_stats)





airline_avg_arr_delay = flights$aggregate('[
                                      { 
                                          "$group" : {
                                          "_id" : {
                                          "AIRLINE" : "$AIRLINE"
                                          }, 
                                          "AVG(ARRIVAL_DELAY)" : {
                                          "$avg" : "$ARRIVAL_DELAY"
                                          }
                                          }
                                          }, 
                                          { 
                                          "$project" : {
                                          "_id" : 0, 
                                          "AIRLINE" : "$_id.AIRLINE", 
                                          "AVG(ARRIVAL_DELAY)" : "$AVG(ARRIVAL_DELAY)"
                                          }
                                          }
                                          ]')

airline_max_arr_delay = flights$aggregate('[
                                          { 
                                          "$group" : {
                                          "_id" : {
                                          "AIRLINE" : "$AIRLINE"
                                          }, 
                                          "MAX(ARRIVAL_DELAY)" : {
                                          "$max" : "$ARRIVAL_DELAY"
                                          }
                                          }
                                          }, 
                                          { 
                                          "$project" : {
                                          "_id" : 0, 
                                          "AIRLINE" : "$_id.AIRLINE", 
                                          "MAX(ARRIVAL_DELAY)" : "$MAX(ARRIVAL_DELAY)"
                                          }
                                          }
                                          ]')

airline_min_arr_delay = flights$aggregate('[
                                          { 
                                          "$group" : {
                                          "_id" : {
                                          "AIRLINE" : "$AIRLINE"
                                          }, 
                                          "MIN(ARRIVAL_DELAY)" : {
                                          "$min" : "$ARRIVAL_DELAY"
                                          }
                                          }
                                          }, 
                                          { 
                                          "$project" : {
                                          "_id" : 0, 
                                          "AIRLINE" : "$_id.AIRLINE", 
                                          "MIN(ARRIVAL_DELAY)" : "$MIN(ARRIVAL_DELAY)"
                                          }
                                          }
                                          ]')


airline_avg_arr_delay$`AVG(ARRIVAL_DELAY)` <- round(airline_avg_arr_delay$`AVG(ARRIVAL_DELAY)`,2)
colnames (airline_avg_arr_delay)[2] <- "Average Delay"
colnames (airline_max_arr_delay)[2] <- "MAX Delay"
colnames (airline_min_arr_delay)[2] <-  "MIN Delay"
flights_general_arr_stats <- cbind(airline_avg_arr_delay,airline_max_arr_delay[2],airline_min_arr_delay[2])
datatable(flights_general_arr_stats)
rm(airline_avg_arr_delay,airline_max_arr_delay,airline_min_arr_delay,flights_general_arr_stats)
#STEP 3.1 END


#3.2 Exploring the monthly distribution of the TOTAL NUMBER OF flights
DAYS = as.data.frame(flights$distinct("DATE"))
flights_Per_Day=flights$aggregate(' [ { "$group" : { "_id" : { "DATE" : "$DATE" }, "COUNT(*)" : 
                                  { "$sum" : 1 } } }, 
                                  { "$project" : { "_id" : 0, "DATE" : "$_id.DATE", "COUNT(*)" : "$COUNT(*)" } }, 
                                  { "$sort" : { "DATE" : 1 } } ]' ) 
DAYS <- as.data.frame(DAYS[order(DAYS$`flights$distinct("DATE")`),])
flights_Per_Day <- na.omit(flights_Per_Day)
names(flights_Per_Day)      <- c("Date","Count")
flights_Per_Day$Date        <- as.Date(flights_Per_Day$Date)
flights_Per_Day$weekday     <- factor(weekdays(flights_Per_Day$Date),levels = unique(weekdays(flights_Per_Day$Date)))
flights_Per_Day$week        <- strftime((flights_Per_Day$Date),format = "%V")
flights_Per_Day$yearmonth   <- factor(as.yearmon(flights_Per_Day$Date )) 
flights_Per_Day$year        <- year(flights_Per_Day$Date)

t <- ggplot(flights_Per_Day, aes(flights_Per_Day$yearmonth, flights_Per_Day$weekday, fill = flights_Per_Day$Count)) + 
  geom_tile(colour = "white") + 
  facet_grid(flights_Per_Day$year) + 
  scale_fill_gradient(low="green", high="red") +
  labs(x="Month Of The Year",
       y="WeekDays",
       title = "Monthly Heatmap of the Total flights", 
       subtitle="Delayed Flights", 
       fill="Count of Total Flights") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

t1 <- ggplot(flights_Per_Day, aes(flights_Per_Day$week, flights_Per_Day$weekday, fill = flights_Per_Day$Count)) + 
  geom_tile(colour = "white") + 
  facet_grid(flights_Per_Day$year) + 
  scale_fill_gradient(low="green", high="red") +
  labs(x="Week Of The Year",
       y="WeekDays",
       title = "Weekly Heatmap of the Total flights", 
       subtitle="Delayed Flights", 
       fill="Count of Total Flights") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))





delayed_flights_per_day <- flights$aggregate( '[ { "$match" : { "$or" : [ { "DEPARTURE_DELAY" : { "$gt" : 0 } }, 
                                       {"ARRIVAL_DELAY" : { "$gt" : 0 } }, 
                                       {"AIR_SYSTEM_DELAY" : { "$gt" : 0 } }, 
                                       {"SECURITY_DELAY" : { "$gt" : 0 } }, 
                                       {"AIRLINE_DELAY" : { "$gt" : 0 } }, 
                                       {"LATE_AIRCRAFT_DELAY" : { "$gt" : 0 } }, 
                                       {"WEATHER_DELAY" : { "$gt" : 0 } } ] } }, 
                                       { "$group" : { "_id" : { "DATE" : "$DATE" }, 
                                       "COUNT(*)" : { "$sum" : 1 } } }, 
                                        { "$project" : { "_id" : 0, "DATE" : "$_id.DATE", "COUNT(*)" : "$COUNT(*)" } }, 
                                        { "$sort" : { "DATE" : 1 } } ]' ) 

delayed_flights_per_day <- na.omit(delayed_flights_per_day)
names(delayed_flights_per_day)      <- c("Date","Count")
delayed_flights_per_day$Date        <- as.Date(delayed_flights_per_day$Date)
delayed_flights_per_day$weekday     <- factor(weekdays(delayed_flights_per_day$Date),levels = unique(weekdays(delayed_flights_per_day$Date)))
delayed_flights_per_day$week        <- strftime((delayed_flights_per_day$Date),format = "%V")
delayed_flights_per_day$yearmonth   <- factor(as.yearmon(delayed_flights_per_day$Date )) 
delayed_flights_per_day$year        <- year(delayed_flights_per_day$Date)

d<-ggplot(delayed_flights_per_day, aes(delayed_flights_per_day$yearmonth, delayed_flights_per_day$weekday, fill = delayed_flights_per_day$Count)) + 
  geom_tile(colour = "white") + 
  facet_grid(delayed_flights_per_day$year) + 
  scale_fill_gradient(low="green", high="red") +
  labs(x="Month Of The Year",
       y="WeekDays",
       title = "Monthly Heatmap of the Delayed flights", 
       subtitle="Delayed Flights", 
       fill="Count of Delayed Flights") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

d1<-ggplot(delayed_flights_per_day, aes(delayed_flights_per_day$week, delayed_flights_per_day$weekday, fill = delayed_flights_per_day$Count)) + 
  geom_tile(colour = "white") + 
  facet_grid(delayed_flights_per_day$year) + 
  scale_fill_gradient(low="green", high="red") +
  labs(x="Week Of The Year",
       y="WeekDays",
       title = "Weekly Heatmap of the Delayed flights", 
       subtitle="Delayed Flights", 
       fill="Count of Delayed Flights") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

grid.arrange(t,d,nrow =1)
grid.arrange(t1,d1,nrow=1)
rm(DAYS,d,d1,t,t1,delayed_flights_per_day)
#STEP 3.2 END 


#STEP 3.3 Flights Distribution over the year
flights_Per_Month = tapply(flights_Per_Day$Count, month(flights_Per_Day$Date), sum)

flights_Per_Month = as.data.frame(cbind(list('January','February','March','April','May','June','July',
                                             'August','September','October','November','December'),
                                        flights_Per_Month))
colnames(flights_Per_Month) <- c("Month","Flights Count")
flights_Per_Month$Month <- as.character((flights_Per_Month$Month))
flights_Per_Month$Month <- factor(flights_Per_Month$Month, levels=unique(flights_Per_Month$Month))
flights_Per_Month$`Flights Count`<- as.integer(flights_Per_Month$`Flights Count`)
ggplot(flights_Per_Month, aes(x=flights_Per_Month$Month, y= flights_Per_Month$`Flights Count`)) + 
  geom_point(size=3) + 
  ylim(0, 300000) +
  geom_segment(aes(x=flights_Per_Month$Month , 
                   xend=flights_Per_Month$Month, 
                   y=0, 
                   yend=flights_Per_Month$`Flights Count` 
                   )) + 
  labs(title="Flights Per Month", 
       subtitle="In Year 2015") + 
  xlab ("Months") +
  ylab ("Flights") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
rm(flights_Per_Day,flights_Per_Month)
#STEP 3.3 END
 


#STEP 3.4 DELAYED CAUSE ANALYSIS
TOTAL_DELAYED_FLIGHTS = flights$aggregate('[ { "$match" : { "$or" : [ { "DEPARTURE_DELAY" : 
                                            { "$gt" :  0 } }, {"ARRIVAL_DELAY" : { "$gt" :  0 } }, 
                                            {"AIR_SYSTEM_DELAY" : { "$gt" :  0 } }, 
                                            {"SECURITY_DELAY" : { "$gt" :  0 } }, 
                                            {"AIRLINE_DELAY" : { "$gt" :  0 } }, 
                                            {"LATE_AIRCRAFT_DELAY" : { "$gt" :  0 } }, 
                                            {"WEATHER_DELAY" : { "$gt" :  0 } } ] } }, 
                                            { "$group" : { "_id" : {  }, "COUNT(*)" : 
                                            { "$sum" :  1 } } }, 
                                            { "$project" : { "_id" :  0, "COUNT(*)" : "$COUNT(*)" } } ]' ) 
AIR_SYSTEM_DELAY = flights$aggregate('[{"$match":{"AIR_SYSTEM_DELAY":{"$gt":0}}},{"$group":{"_id": 
                           {},"COUNT(*)":{"$sum":1}}},{"$project":{"_id":0,"COUNT(*)":"$COUNT(*)"}}]')
SECURITY_DELAY = flights$aggregate('[{"$match":{"SECURITY_DELAY":{"$gt":0}}},{"$group":{"_id": 
                           {},"COUNT(*)":{"$sum":1}}},{"$project":{"_id":0,"COUNT(*)":"$COUNT(*)"}}]')
AIRLINE_DELAY = flights$aggregate('[{"$match":{"AIRLINE_DELAY":{"$gt":0}}},{"$group":{"_id": 
                           {},"COUNT(*)":{"$sum":1}}},{"$project":{"_id":0,"COUNT(*)":"$COUNT(*)"}}]')
LATE_AIRCRAFT_DELAY = flights$aggregate('[{"$match":{"LATE_AIRCRAFT_DELAY":{"$gt":0}}},{"$group":{"_id": 
                           {},"COUNT(*)":{"$sum":1}}},{"$project":{"_id":0,"COUNT(*)":"$COUNT(*)"}}]')
WEATHER_DELAY = flights$aggregate('[{"$match":{"WEATHER_DELAY":{"$gt":0}}},{"$group":{"_id": 
                           {},"COUNT(*)":{"$sum":1}}},{"$project":{"_id":0,"COUNT(*)":"$COUNT(*)"}}]')

df = rbind(AIR_SYSTEM_DELAY,SECURITY_DELAY,AIRLINE_DELAY,LATE_AIRCRAFT_DELAY,WEATHER_DELAY)
df_names = rbind("AIR_SYSTEM_DELAY","SECURITY_DELAY","AIRLINE_DELAY","LATE_AIRCRAFT_DELAY","WEATHER_DELAY")
df = cbind(df_names,df)
df = rbind(df) 
colnames(df) <- c("DELAY CAUSE","COUNT")

ggplot(df, aes(x=df$`DELAY CAUSE`, y=df$COUNT)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") +
  geom_text(aes(label=df$COUNT), vjust=-0.3, size=3.5) +
  xlab ("Delay Causes") +
  ylab ("Number of Flights") +
  ylim(0,300000) +
  labs(title="Delay Causes", 
       subtitle="", 
       caption="") + 
  theme(axis.text.x = element_text(angle=20, vjust=0.6))
rm(df,AIRLINE_DELAY,AIR_SYSTEM_DELAY,AIRLINE_DELAY,delay_rate_by_airlines,LATE_AIRCRAFT_DELAY,SECURITY_DELAY,WEATHER_DELAY)
#STEP 3.4 END


#STEP 3.5 AIRLINE DELAY ANALYSIS 
delay_rate_by_airlines = flights$aggregate(' [ { "$match" : { "$or" : 
                                           [ { "DEPARTURE_DELAY" : { "$gt" : 0 } }, 
                                           {"ARRIVAL_DELAY" : { "$gt" : 0 } }, 
                                           {"AIR_SYSTEM_DELAY" : { "$gt" : 0 } }, 
                                           {"SECURITY_DELAY" : { "$gt" : 0 } }, 
                                           {"AIRLINE_DELAY" : { "$gt" : 0 } }, 
                                           {"LATE_AIRCRAFT_DELAY" : { "$gt" : 0 } }, 
                                           {"WEATHER_DELAY" : { "$gt" : 0 } } ] } }, 
                                           { "$group" : { "_id" : { "AIRLINE_ID" : "$AIRLINE_ID" }, 
                                           "COUNT(*)" : { "$sum" :  1 } } }, 
                                           { "$project" : { "_id" :  0, "AIRLINE_ID" : "$_id.AIRLINE_ID", 
                                           "COUNT(*)" : "$COUNT(*)" } }, { "$sort" : { "AIRLINE_ID" :  1 } } ]') 
delay_rate_by_airlines <- na.omit(delay_rate_by_airlines)
delay_rate_by_airlines$Delay = round(((delay_rate_by_airlines$`COUNT(*)`/TOTAL_DELAYED_FLIGHTS$`COUNT(*)`) * 100),2)
colnames(delay_rate_by_airlines)[3] <- "DELAY in %" 
colnames(delay_rate_by_airlines)[2] <- "Delayed Flight Count" 
airlines = mongo(collection = "Airlines", db= "Assignment3")
airlines_data <- airlines$find('{}')
airlines_data <- na.omit(airlines_data)
i=1
j=1
for (i in 1:nrow(airlines_data))
  {
  for (j in 1:nrow(delay_rate_by_airlines))
  {
   if(airlines_data$IATA_CODE[i] == delay_rate_by_airlines$AIRLINE_ID[j])
     delay_rate_by_airlines$AIRLINE_ID[j] <- airlines_data$AIRLINE[i]
   else
     next(i)
 }
}

Delayed_flights_Count = delay_rate_by_airlines$`Delayed Flight Count`
Airline_ID = delay_rate_by_airlines$AIRLINE_ID
#delay_rate_by_airlines$Delay_Rate = delay_rate_by_airlines$`DELAY in %`
qplot(data = delay_rate_by_airlines, 
      x = delay_rate_by_airlines$`DELAY in %`, 
      y = delay_rate_by_airlines$AIRLINE_ID, 
      size = Delayed_flights_Count, 
      color = Airline_ID) +
  xlab ("Delay in %") +
  ylab ("Airline Name") +
  theme(axis.text.x = element_text(angle=0, vjust=0.6))
rm(Delayed_flights_Count,Airline_ID,i,j,Delayed_flights,TOTAL_DELAYED_FLIGHTS,delay_rate_by_airlines,airlines_data)
rm(df_names)
#STEP 3.5 END




#STEP 3.6 ORIGIN - DESTINATION DELAY STATISTICS (DATASET FOR TABLEAU EXPLORATION) 
flights_by_origin_Dest_pair = flights$aggregate(' [ { "$match" : { "$or" : 
                                                [ { "DEPARTURE_DELAY" : { "$gt" : 0 } }, 
                                                {"ARRIVAL_DELAY" : { "$gt" : 0 } }, 
                                                {"AIR_SYSTEM_DELAY" : { "$gt" : 0 } }, 
                                                {"SECURITY_DELAY" : { "$gt" : 0 } }, 
                                                {"AIRLINE_DELAY" : { "$gt" : 0 } }, 
                                                {"LATE_AIRCRAFT_DELAY" : { "$gt" : 0 } }, 
                                                {"WEATHER_DELAY" : { "$gt" : 0 } } ] } }, 
                                                { "$group" : { "_id" : 
                                                { "ORIG_AIRPORTID" : "$ORIG_AIRPORTID", 
                                                "DEST_AIRPORTID" : "$DEST_AIRPORTID", 
                                                "ORIG_LATITUDE" : "$ORIG_LATITUDE", 
                                                "ORIG_LONGITUDE" : "$ORIG_LONGITUDE", 
                                                "DEST_LATITUDE" : "$DEST_LATITUDE",
                                                "DEST_LONGITUDE" : "$DEST_LONGITUDE", 
                                                "AIRLINE" : "$AIRLINE", 
                                                "ORIG_AIRPORT_CITY" : "$ORIG_AIRPORT_CITY",
                                                "DEST_AIRPORT_CITY" : "$DEST_AIRPORT_CITY" }, 
                                                "COUNT(*)" : { "$sum" : 1 } } }, 
                                                { "$project" : { "_id" : 0, 
                                                "ORIG_AIRPORTID" : "$_id.ORIG_AIRPORTID", 
                                                "DEST_AIRPORTID" : "$_id.DEST_AIRPORTID",
                                                "ORIG_LATITUDE" :  "$_id.ORIG_LATITUDE", 
                                                "ORIG_LONGITUDE" : "$_id.ORIG_LONGITUDE", 
                                                "DEST_LATITUDE" : "$_id.DEST_LATITUDE", 
                                                "DEST_LONGITUDE" :  "$_id.DEST_LONGITUDE", 
                                                "AIRLINE" : "$_id.AIRLINE", 
                                                "ORIG_AIRPORT_CITY" : "$_id.ORIG_AIRPORT_CITY", 
                                                "DEST_AIRPORT_CITY" : "$_id.DEST_AIRPORT_CITY",
                                                "COUNT(*)" : "$COUNT(*)" } } ]' ) 
flights_by_origin_Dest_pair = flights_by_origin_Dest_pair[!is.na(flights_by_origin_Dest_pair$ORIG_LATITUDE), ]
flights_by_origin_Dest_pair = flights_by_origin_Dest_pair[!is.na(flights_by_origin_Dest_pair$ORIG_LONGITUDE), ]
 flights_by_origin_Dest_pair = flights_by_origin_Dest_pair[!is.na(flights_by_origin_Dest_pair$DEST_LATITUDE), ]
 flights_by_origin_Dest_pair = flights_by_origin_Dest_pair[!is.na(flights_by_origin_Dest_pair$DEST_LONGITUDE), ]

 write.csv(flights_by_origin_Dest_pair,file = "Origin & Delay Density Analysis.csv")
rm(flights_by_origin_Dest_pair)
#STEP 3.6 END


#STEP 3.7 Odd time delay analysis
odd_time <- flights$aggregate(' [ { "$match" : { "$or" : [ { "$and" : 
                            [ { "$or" : [ { "ARRIVAL_DELAY" : { "$gt" : 0 } }, 
                              {"AIR_SYSTEM_DELAY" : { "$gt" : 0 } }, 
                              {"SECURITY_DELAY" : { "$gt" : 0 } }, 
                              {"AIRLINE_DELAY" : { "$gt" : 0 } }, 
                              {"LATE_AIRCRAFT_DELAY" : { "$gt" : 0 } }, 
                              {"WEATHER_DELAY" : { "$gt" : 0 } }, 
                              {"DEPARTURE_DELAY" : { "$gt" : 0 } } ] }, 
                              {"DEPARTURE_TIME" : { "$gt" : 1 } }, 
                              {"DEPARTURE_TIME" : { "$lt" : 600 } } ] }, 
                              {"DEPARTURE_TIME" : { "$gt" : 2200 } } ] } }, 
                              { "$group" : { "_id" : { "AIRLINE" : "$AIRLINE" }, 
                              "COUNT(DEPARTURE_DELAY)" : { "$sum" :  1 }}}, 
                              { "$project" : { "_id" : 0, "AIRLINE" : "$_id.AIRLINE", 
                              "COUNT(DEPARTURE_DELAY)" : "$COUNT(DEPARTURE_DELAY)" } },{ 
                              "$sort" : {
                              "AIRLINE" : 1 
                              }
                              } ]' ) 
  
total_flights <- flights$aggregate(' [ { "$group" : { "_id" : { "AIRLINE" : "$AIRLINE" }, 
                                   "COUNT(*)" : { "$sum" :  1 }}}, 
                                   { "$project" : { "_id" :  0, "AIRLINE" : "$_id.AIRLINE", "COUNT(*)" : 
                                   "$COUNT(*)" } },{ 
                                   "$sort" : {
                                   "AIRLINE" : 1
                                   }
                                   } ]' ) 
  
total_delayed_by_airline <- flights$aggregate(' [ { "$match" : { "$or" : 
                                              [ { "ARRIVAL_DELAY" : { "$gt" : 0 } }, {"AIR_SYSTEM_DELAY" : { "$gt" : 0 } }, 
                                              {"SECURITY_DELAY" : { "$gt" : 0 } }, {"AIRLINE_DELAY" : { "$gt" : 0 } }, 
                                              {"LATE_AIRCRAFT_DELAY" : { "$gt" : 0 } }, {"WEATHER_DELAY" : { "$gt" : 0 } }, 
                                              {"DEPARTURE_DELAY" : { "$gt" : 0 } } ] } }, { "$group" : { "_id" : { "AIRLINE" : "$AIRLINE" }, 
                                              "COUNT(*)" : { "$sum" : 1 } } }, { "$project" : { "_id" : 0, "AIRLINE" : "$_id.AIRLINE", 
                                              "COUNT(*)" : "$COUNT(*)" } },{ 
                                              "$sort" : {
                                              "AIRLINE" : 1
                                              }
                                              } ]') 

Odd_Time_delay_analysis <- cbind(total_flights,total_delayed_by_airline[2],odd_time[2])
names(Odd_Time_delay_analysis)[2] <- "Total Flights"
names(Odd_Time_delay_analysis)[3] <- "Total Delayed Flights"
names(Odd_Time_delay_analysis)[4] <- "Flights Delayed at Odd times( > 10PM and < 6AM)"

Odd_Time_delay_analysis$percent_of_tot_flights <-   round((Odd_Time_delay_analysis$`Flights Delayed at Odd times( > 10PM and < 6AM)`
                                                           / Odd_Time_delay_analysis$`Total Flights`) * 100,2)
Odd_Time_delay_analysis$percent_of_delayed_flights <-   round((Odd_Time_delay_analysis$`Flights Delayed at Odd times( > 10PM and < 6AM)`
                                                               / Odd_Time_delay_analysis$`Total Delayed Flights`) * 100,2)  
write.csv(Odd_Time_delay_analysis,file = "Odd_Time_delay_analysis.csv")
rm(total_delayed_by_airline,total_flights,odd_time,Odd_Time_delay_analysis)
#STEP 3.7 END






#STEP 3.8 TAXI IN AND TAXI OUT TIME VS DELAY ANALYSIS
temp = flights$find('{}',
                    fields = 
                        ' { 
                          "DATE" : "$DATE",
                          "AIRLINE_ID" : "$AIRLINE_ID", 
                          "AIRLINE" : "$AIRLINE", 
                          "DEPARTURE_DELAY" : "$DEPARTURE_DELAY", 
                          "ARRIVAL_DELAY" : "$ARRIVAL_DELAY", 
                          "TAXI_OUT" : "$TAXI_OUT", 
                          "TAXI_IN" : "$TAXI_IN",
                          "WHEELS_OFF" : "$WHEELS_OFF",
                          "WHEELS_ON" : "$WHEELS_ON"
                          }
')


dep_delay  = aggregate(temp$DEPARTURE_DELAY, by=list(temp$DATE), FUN=mean)
arr_delay  = aggregate(temp$ARRIVAL_DELAY, by=list(temp$DATE), FUN=mean)
taxi_out   =  aggregate(temp$TAXI_OUT, by=list(temp$DATE), FUN=mean)
taxi_in    =   aggregate(temp$TAXI_IN, by=list(temp$DATE), FUN=mean)
dep_delay$Group.1 <- as.Date(dep_delay$Group.1)
arr_delay$Group.1 <- as.Date(arr_delay$Group.1)
taxi_in$Group.1 <- as.Date(taxi_in$Group.1) 
taxi_out$Group.1 <- as.Date(taxi_out$Group.1) 
plot1=  ggplot(dep_delay,aes(dep_delay$x,dep_delay$Group.1)) + 
  geom_point() + xlim(0,50)+
  xlab("Departure Delay in Minutes") +
  ylab("Month") +
  scale_color_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1))

plot2=  ggplot(arr_delay,aes(arr_delay$x,arr_delay$Group.1)) + 
    geom_point() +xlim(0,50)+ 
  xlab("Arrival Delay in Minutes") +
  ylab("Month") +
    scale_color_manual(values = c('#999999','#E69F00')) + 
    theme(legend.position=c(0,1), legend.justification=c(0,1))
  
plot3=  ggplot(taxi_in,aes(taxi_in$x,taxi_in$Group.1)) + 
    geom_point() + 
  xlab("Taxi in Time Distribution in Minutes") +
  ylab("Month") +
    scale_color_manual(values = c('#999999','#E69F00')) + 
    theme(legend.position=c(0,1), legend.justification=c(0,1))
  
plot4=  ggplot(taxi_out,aes(taxi_out$x,taxi_out$Group.1)) + 
    geom_point() + 
  xlab("Taxi out Time Distribution in Minutes") +
  ylab("Month") +
    scale_color_manual(values = c('#999999','#E69F00')) + 
    theme(legend.position=c(0,1), legend.justification=c(0,1))



grid.arrange(plot1,plot2,
             plot4,plot3,nrow =2,ncol=2)
  
rm(plot1,plot2,plot3,plot4,taxi_in,taxi_out,dep_delay,arr_delay,temp)

#STEP 3.8 END




#STEP 3.9 TOP 10 ANALYSIS OF AIRPORT AND CITY


#__________________
top_10_orig_city <- flights$aggregate(' [ { "$match" : { "$or" : 
                                      [ { "ARRIVAL_DELAY" : { "$gt" : 0 } }, 
                                      {"AIR_SYSTEM_DELAY" : { "$gt" : 0 } }, 
                                      {"SECURITY_DELAY" : { "$gt" : 0 } }, 
                                      {"AIRLINE_DELAY" : { "$gt" : 0 } }, 
                                      {"LATE_AIRCRAFT_DELAY" : { "$gt" : 0 } }, 
                                      {"WEATHER_DELAY" : { "$gt" : 0 } }, 
                                      {"DEPARTURE_DELAY" : { "$gt" : 0 } } ] } }, 
                                      { "$group" : { "_id" : 
                                      { "ORIG_AIRPORT_CITY" : "$ORIG_AIRPORT_CITY" }, 
                                      "COUNT(*)" : { "$sum" : 1 } } },
                                      { "$project" : { "_id" : 0, 
                                      "ORIG_AIRPORT_CITY" : "$_id.ORIG_AIRPORT_CITY", 
                                      "COUNT(*)" : "$COUNT(*)" } }, 
                                      { "$sort" : { "COUNT(*)" : -1 } }, { "$limit" : 10 } ]' ) 
 

top_10_dest_city <- flights$aggregate(' [ { "$match" : { "$or" : 
                                      [ { "ARRIVAL_DELAY" : { "$gt" : 0 } }, 
                                      {"AIR_SYSTEM_DELAY" : { "$gt" : 0 } }, 
                                      {"SECURITY_DELAY" : { "$gt" : 0 } }, 
                                      {"AIRLINE_DELAY" : { "$gt" : 0 } }, 
                                      {"LATE_AIRCRAFT_DELAY" : { "$gt" : 0 } }, 
                                      {"WEATHER_DELAY" : { "$gt" : 0 } }, 
                                      {"DEPARTURE_DELAY" : { "$gt" : 0 } } ] } }, 
                                      { "$group" : { "_id" : 
                                      { "DEST_AIRPORT_CITY" : "$DEST_AIRPORT_CITY" }, 
                                      "COUNT(*)" : { "$sum" : 1 } } },
                                      { "$project" : { "_id" : 0, 
                                      "DEST_AIRPORT_CITY" : "$_id.DEST_AIRPORT_CITY", 
                                      "COUNT(*)" : "$COUNT(*)" } }, 
                                      { "$sort" : { "COUNT(*)" : -1 } }, { "$limit" : 10 } ]' ) 

write.csv(top_10_orig_city,file = "Top 10 Busy Origin Airports.csv")
write.csv(top_10_dest_city,file = "Top 10 Busy Destination Airports.csv")
rm(top_10_orig_city,top_10_dest_city)
#STEP 3.9 END

#################################################STEP 3 END#######################################################




#################################################STEP 4 FORECASTING#######################################################
  library('forecast')
  library('tseries')
  #STEP 1 GET THE Mean Delay Statistics from MONGODB
  flights = mongo(collection = "Flights_Subset",db="Assignment3")
  arr_del_forecast_data <- flights$aggregate(' [ { "$group" : { "_id" : { "DATE" : "$DATE" }, 
                                     "AVG(ARRIVAL_DELAY)" : { "$avg" : "$ARRIVAL_DELAY" } } }, { "$project" : { "_id" : 0, "DATE" : "$_id.DATE", "AVG(ARRIVAL_DELAY)" : "$AVG(ARRIVAL_DELAY)" } }, { "$sort" : { "DATE" : 1 } } ]' ) 
  dep_del_forecast_data <- flights$aggregate(' [ { "$group" : { "_id" : { "DATE" : "$DATE" }, 
                                     "AVG(DEPARTURE_DELAY)" : { "$avg" : "$DEPARTURE_DELAY" } } }, { "$project" : { "_id" : 0, "DATE" : "$_id.DATE", "AVG(DEPARTURE_DELAY)" : "$AVG(DEPARTURE_DELAY)" } }, { "$sort" : { "DATE" : 1 } } ]' ) 
  
  #STEP 2 CREATE TIME SERIES FOR TRAINING THE MODEL FOR QUARTERLY FORECASTING OF DEPARTURE DELAY
  time_series <- ts(dep_del_forecast_data[,2],frequency=90,start = c(1),end = c(5))
  fit <- HoltWinters(time_series)
  # predict next three future values
  
  #STEP 3 FORECAST FOR NEXT ONE QUARTER
  forecast(fit, 90)
  plot(forecast(fit, 90))
  autoplot(fit)
  
  
  
  #STEP 4 CREATE TIME SERIES FOR TRAINING THE MODEL FOR QUARTERLY FORECASTING OF DEPARTURE DELAY
  time_series <- ts(arr_del_forecast_data[,2],frequency=90,start = c(1),end = c(5))
  fit <- HoltWinters(time_series)
  # predict next three future values
  #STEP 5 CREATE TIME SERIES FOR TRAINING THE MODEL FOR QUARTERLY FORECASTING OF DEPARTURE DELAY
  forecast(fit, 90)
  plot(forecast(fit, 90))
  autoplot(fit)
  
  rm(dep_del_forecast_data,arr_del_forecast_data,time_series,fit)
  #################################################STEP 4 END#######################################################
  

  
  #######################################ANALYSIS END############################################################
  