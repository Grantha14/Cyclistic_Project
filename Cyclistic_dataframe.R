library(tidyverse)
library(ggplot2)
library(readr)
library(skimr)
library(janitor)
library(tidyr)
library(lubridate)
library(dplyr)
library(data.table)
library(hms)
library(plotly)

jan23_df<-read_csv("C://Users//GRANTHA//Documents//Cyclistic_bikes_2024//.csv files//202301-divvy-tripdata.csv")
feb23_df<-read_csv("C://Users//GRANTHA//Documents//Cyclistic_bikes_2024//.csv files//202302-divvy-tripdata.csv")
mar23_df<-read_csv("C://Users//GRANTHA//Documents//Cyclistic_bikes_2024//.csv files//202303-divvy-tripdata.csv")
apr23_df<-read_csv("C://Users//GRANTHA//Documents//Cyclistic_bikes_2024//.csv files//202304-divvy-tripdata.csv")
may23_df<-read_csv("C://Users//GRANTHA//Documents//Cyclistic_bikes_2024//.csv files//202305-divvy-tripdata.csv")
jun23_df<-read_csv("C://Users//GRANTHA//Documents//Cyclistic_bikes_2024//.csv files//202306-divvy-tripdata.csv")
jul23_df<-read_csv("C://Users//GRANTHA//Documents//Cyclistic_bikes_2024//.csv files//202307-divvy-tripdata.csv")
aug23_df<-read_csv("C://Users//GRANTHA//Documents//Cyclistic_bikes_2024//.csv files//202308-divvy-tripdata.csv")
sep23_df<-read_csv("C://Users//GRANTHA//Documents//Cyclistic_bikes_2024//.csv files//202309-divvy-tripdata.csv")
oct3_df<-read_csv("C://Users//GRANTHA//Documents//Cyclistic_bikes_2024//.csv files//202310-divvy-tripdata.csv")
nov23_df<-read_csv("C://Users//GRANTHA//Documents//Cyclistic_bikes_2024//.csv files//202311-divvy-tripdata.csv")
dec23_df<-read_csv("C://Users//GRANTHA//Documents//Cyclistic_bikes_2024//.csv files//202312-divvy-tripdata.csv")


#combining all month datasets into one dataframe
cyclistic_df<-rbind(jan23_df,feb23_df,mar23_df,apr23_df,may23_df,jun23_df,jul23_df,
                    aug23_df,sep23_df,oct3_df,nov23_df,dec23_df)

View(cyclistic_df)
#save as rds file
saveRDS(cyclistic_df, file = "cyclistic_df.rds")

#types of rides and customers
rideable_types<-unique(cyclistic_df$rideable_type)
print(rideable_types)
member_types<-unique(cyclistic_df$member_casual)
print(member_types)

#Data Manipulation
#trimmed dataframe including only relevant columns/variables
trimmed_cyclistic_df<-select(cyclistic_df,"rideable_type","started_at", "ended_at","member_casual")

#create a dataframe including the new columns
cyclistic_new_df<-trimmed_cyclistic_df
View(cyclistic_new_df)
saveRDS(cyclistic_new_df, file = "cyclistic_new_df.rds")

#create a new column for ride_length(which we can calculate with the timings)
#starting with the dataframe name 'cyclistic_new_df' so that we are creating the column within that table
cyclistic_new_df$ride_length <- difftime(cyclistic_new_df$ended_at,cyclistic_new_df$started_at,units = "mins")
cyclistic_new_df$ride_length <- round(cyclistic_new_df$ride_length, digits = 1)

#calculations
#We are using only the started_at to calculate
#Convert the date portion of 'started_at' column to a Date object and stores it in a new column called 'date'.
cyclistic_new_df$date <- as.Date(cyclistic_new_df$started_at)

#Calculate the day of the week from the date portion of 'started_at' column and store it in a new column called 'day_of_week'
cyclistic_new_df$day_of_week <- weekdays(cyclistic_new_df$started_at)

#Extract Day, Month, Year
#Reformat the 'day_of_week' column by extracting the day of the week from the 'date' column, ensuring the full name of the day (like "Monday") is captured.
cyclistic_new_df$day_of_week <- format(as.Date(cyclistic_new_df$date), "%A")

#Extract the month from the 'date' column and store it as a new column 'month'.
cyclistic_new_df$month <- format(as.Date(cyclistic_new_df$date), "%m")

#Extract the day from the 'date' column and store it as a new column 'day'.
cyclistic_new_df$day <- format(as.Date(cyclistic_new_df$date), "%d")

#Extract the year from the 'date' column and store it as a new column 'year'.
cyclistic_new_df$year <- format(as.Date(cyclistic_new_df$date), "%Y")

#Extract Time and Hour
#Extract the time (hours, minutes, and seconds) from the 'date' column.
cyclistic_new_df$time <- format(as.Date(cyclistic_new_df$date), "%H:%M:%S")

#Corrects the time extraction by using the 'as_hms()' function (from the hms package), which extracts the time part of the 'started_at' column and stores it in the 'time' column.
cyclistic_new_df$time <- as_hms(cyclistic_new_df$started_at)

#Extracts the hour from the 'time' column and stores it in a new column called 'hour'.
cyclistic_new_df$hour <- hour(cyclistic_new_df$time)

#Map months to different seasons
cyclistic_new_df$season <- ifelse(cyclistic_new_df$month %in% c("12", "01", "02"), "Winter",
                                  ifelse(cyclistic_new_df$month %in% c("03", "04", "05"), "Spring",
                                         ifelse(cyclistic_new_df$month %in% c("06", "07", "08"), "Summer",
                                                "Fall")))

#Removes any rows in the dataframe that contain NA values.
cyclistic_new_df <- na.omit(cyclistic_new_df) 

#Removes any duplicate rows from the dataframe.
cyclistic_new_df <- distinct(cyclistic_new_df) 

#Remove negative values from 'ride_length'
cyclistic_new_df <- cyclistic_new_df[!(cyclistic_new_df$ride_length <= 0),] 

cyclistic_new_df2 <- cyclistic_new_df
View(cyclistic_new_df2)
saveRDS(cyclistic_new_df2,"cyclistic_new_df2.rds")

#Calculated stats
#Total number of rides = 5,706,996
total_rides <- nrow(cyclistic_new_df2)
print(total_rides)

#Average ride length = 18.22478 mins
average_ride_length <- mean(cyclistic_new_df2$ride_length)
print(average_ride_length)

#Busiest time/hour of day = 17 / 5:00 PM
rides_per_hour <- table(cyclistic_new_df2$hour) #Use the table() function to count the number of rides(rows) for each hour.
print(rides_per_hour)
  
busiest_hour <- names(which.max(rides_per_hour))
print(busiest_hour)

#Busiest day of the week = Saturday (rides per day) , if you put 'day' it would be rides per date.
rides_per_day<- table(cyclistic_new_df2$day_of_week)
print(rides_per_day)

busiest_day <- names(which.max(rides_per_day))
print(busiest_day)

#Busiest month = 08 which is August
rides_per_month <- table(cyclistic_new_df2$month)
print(rides_per_month)

busiest_month <- names(which.max(rides_per_month))
print(busiest_month)

#Busiest season = Summer
rides_per_season <- table(cyclistic_new_df2$season)
print(rides_per_season)

busiest_season <- names(which.max(rides_per_season))
print(busiest_season)

#Most popular bike = Electric Bike
rides_per_bike_type <- table(cyclistic_new_df2$rideable_type)
print(rides_per_bike_type)

popular_bike <- names(which.max(rides_per_bike_type))
print(popular_bike)


#Making Visualizations for Members vs Casual
#Total number of rides for each customer type casual = 2,055,213, member = 3,651,783
p1 <- cyclistic_new_df2 %>% 
  group_by(member_casual) %>% 
  summarize(total_rides = n()) %>% 
  arrange(member_casual) %>% 
  ggplot(aes(x = member_casual, y = total_rides, fill=member_casual)) +
  labs(title = "Total Rides By Customer Type",x = "Customer type", y="Total number of rides") +
  geom_col(width = 0.5,position = position_dodge(width=0.5)) + #position_dodge(): This is a function that controls the horizontal displacement of elements to prevent them from overlapping. 
  #It’s commonly used in bar charts or grouped bar charts to separate bars of different groups. width=0.5: This argument specifies the amount of dodge (i.e., the distance between the elements of different groups, here it is 0.5).
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) 
#scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) ensures that the y-axis labels are shown in standard decimal notation, avoiding scientific notation. This makes the plot easier to interpret when the values are large.
  ggplotly(p1)
  
#Average ride length by customer type casual = 28.30 mins, member = 12.55 mins
p2 <- cyclistic_new_df2 %>%
  group_by(member_casual)%>%
  summarize(average_ride_duration = mean(ride_length))%>%
  ggplot(aes(x = member_casual, y = average_ride_duration, fill = member_casual)) +
  labs(title = "Average Ride Length By Customer Type", x= "Customer Type",y="Average ride length") +
  geom_col(width = 0.5, position = position_dodge(width = 0.5))
ggplotly(p2)

#Busiest time by customer type(line chart)(5pm for both)
p3 <- cyclistic_new_df2 %>%
  group_by(member_casual, hour)%>%
  summarize(total_rides = n())%>%
  ggplot(aes(x = hour, y = total_rides, color = member_casual, group = member_casual)) +
  geom_line() +
  labs(title = "Bike Demand by Hour", x = "Time of Day",y = "Number of Rides") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
ggplotly(p3)

#Busiest weekday by customer type(grouped bar plot)(member = thursday,casual = saturday)
p4 <- cyclistic_new_df2 %>%
  group_by(member_casual, day_of_week)%>%
  summarize(total_rides = n())%>%
  ggplot(aes(x = day_of_week, y = total_rides, fill = member_casual, group = member_casual)) +
  labs(title = "Total Rides by Weekday",x = "Days of the Week", y = "Number of Rides") +
  geom_col(width = 0.5, position = position_dodge(width = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
ggplotly(p4)

#run this first for the weekdays to be displayed from Monday-Sunday
cyclistic_new_df2 <- cyclistic_new_df2 %>%
  mutate(day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

#Busiest Month by customer type(grouped bar plot)(member  = august ,casual = july )
ggplotly(
  cyclistic_new_df2 %>%
    group_by(member_casual, month)%>%
    summarize(total_rides = n())%>%
    arrange(member_casual, month)%>%
    ggplot(aes(x = month, y = total_rides, fill = member_casual)) +
    labs(title = "Total Rides by Month") +
    theme(axis.text.x = element_text(angle = 30)) +
    geom_col(width = 0.5, position = position_dodge(width = 0.5)) +
    scale_fill_manual(values = c("member" = "#1a5276", "casual" = "#f8c471")) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
)

#Busiest Season by customer type(stacked bar plot)(summer for both)
#print if the plot is not displayed(common in R when we store plot in an object , try calling the object(p5) explicitly)
p5<-cyclistic_new_df2 %>% 
  group_by(member_casual,season) %>% 
  summarize(total_rides = n()) %>% 
  ggplot(aes(x = season, y = total_rides,fill = member_casual)) +
  geom_bar(stat = "identity") +
  labs(title = "Busiest Season by Customer Type",x = "Season", y = "Number of Rides") +
  theme_minimal() +
  scale_fill_manual(values = c("member" = "#1a5276", "casual" = "#f8c471"))
ggplotly(p5)

#Most popular bikes by customer type(electric for both)
#can also use ggplotly like this
ggplotly(
  cyclistic_new_df2 %>%
  group_by(rideable_type, member_casual)%>%
  summarize(total_rides = n())%>%
  ggplot(aes(x = rideable_type, y = total_rides, fill = member_casual)) +
  geom_bar(stat = 'identity') +
  labs(title = "Total Rides by Bike Type", x = "Bike Type", y = "Number of Rides") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_fill_manual(values = c( "member" = "#0d47a1","casual" = "#e53935")))
  
#Ride length by weekday
ggplotly(
  cyclistic_new_df2 %>% 
    group_by(member_casual,day_of_week) %>% 
    summarize(average_ride_length = mean(ride_length)) %>% 
    ggplot(aes(x = day_of_week, y = average_ride_length,color = member_casual, group = member_casual)) +
    geom_line(size = 1.0) +
    facet_wrap(~member_casual) +
    labs(title = "Average Ride Length by Weekday", x = "Days of the Week", y = "Average Ride Length") +
    scale_y_continuous(labels = function(x) format(x, scientific =  FALSE)) 
)
