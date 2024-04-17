#setwd("D:/Real Documents/old desktop/Google Analytics/capstone/Track A_Case 1/extracted data/202402-divvy-tripdata")


#tripdata_202402 <- read_csv('202402-divvy-tripdata.csv')

#my_data <- list(tripdata_202302, tripdata_202303, tripdata_202304, tripdata_202305, tripdata_202306, tripdata_202307, tripdata_202308, 
#                tripdata_202309, tripdata_202310, tripdata_202311, tripdata_202312, tripdata_202401)


load("Cyclistic_202302-202401.RData")

install.packages('tidyverse')
library(tidyverse)
library(ggplot2)
library(lubridate)


#binding all the dataframes together for the year, We'll start with 2 months to see how it does first
beep <- rbind(tripdata_202302, tripdata_202303, tripdata_202304, tripdata_202305, tripdata_202306, tripdata_202307, tripdata_202308, 
              tripdata_202309, tripdata_202310, tripdata_202311, tripdata_202312, tripdata_202401)

#adding a ride time column, removing negative ride times, adding change in lat and long
beep <- drop_na(beep) %>%
  mutate(ride_time = ended_at-started_at) %>%
  filter(ride_time > 5 && ride_time < 86400) %>%
  mutate(dist_lat = abs(end_lat - start_lat)) %>%
  mutate(dist_lng = abs(end_lng - start_lng)) %>%
  mutate(dist_total = sqrt((dist_lat*dist_lat)+(dist_lng*dist_lng))) %>%
  filter(beep, dist_total <1) # remove where the distance is huge for some reason

# change the docked bikes to classic bikes 
beep['rideable_type'][beep['rideable_type'] == "docked_bike"] <- "classic_bike"

c <- beep %>% group_by(member_casual, rideable_type) %>% 
  summarize(max_ride_t=max(ride_time), mean_ride_t=mean(ride_time),max_ride_d=max(dist_total), mean_ride_d=mean(dist_total), total_rides = n())

d <- beep %>% group_by(month(started_at), member_casual) %>% 
  summarize(max_ride_t=max(ride_time), med_ride_t=median(ride_time),max_ride_d=max(dist_total), med_ride_d=median(dist_total), total_rides = n())

# Bar graph of the total rides per month for both casual and member riders 
ggplot(data=d)+
  geom_col(mapping=aes(x=`month(started_at)`, y=`total_rides`, fill=`member_casual`), position = 'dodge')+
  labs(title='Rides per Month by member/casual')


#Have to add what I did on Sam's laptop - should start using Git more 
# still want to do that scatter but have to split the summarized data to contain both 



# create vectors to be filled with the average ride times per month
casual_time <- vector(length=12)
member_time <- vector(length=12)

# use a for loop to go month by month and taking the mean ride times
for (i in 1:12) {
  casual_time[i] = d$med_ride_t[2*i-1]
  member_time[i] = d$med_ride_t[2*i]
}

# making a breakdown of the monthly averages per member type
month_break <- data.frame(casual_time,member_time)

# plotting the times on x and y with the equivalency line plotted 
ggplot(data=month_break, aes(x=member_time, y=casual_time))+
  geom_point()+
  geom_abline(intercept=0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Member Time", y = "Casual Time", title = "Monthly ride times of Casuals vs Members") +  # Add axis labels and title
  theme_minimal() +
  coord_cartesian(xlim = c(0, 1600), ylim = c(0, 1600))  # Set plot limits

#want to group by location?

# meep <- beep %>% group_by(start_station_name, rideable_type) %>% 
#  summarize(max_ride_t=max(ride_time), mean_ride_t=mean(ride_time),max_ride_d=max(dist_total), mean_ride_d=mean(dist_total), total_rides = n())

# Assuming your data frame is named 'df'

# Group by one column and summarize conditionally based on another column
meed <- beep %>%
# meen <- beep %>%  
  group_by(start_station_name) %>%
  summarize(
    med_t_member = median(if_else(member_casual == "member", as.double(ride_time), NA), na.rm = TRUE),
    med_t_casual = median(if_else(member_casual == "casual", as.double(ride_time), NA), na.rm = TRUE),
#    mean_t_member = mean(if_else(member_casual == "member", as.double(ride_time), 0)),
#    mean_t_casual = mean(if_else(member_casual == "casual", as.double(ride_time), 0)),
    count_member = sum(member_casual=="member"),
    count_casual = sum(member_casual=="casual"),
    count_dif = (count_member-count_casual),
    count_total = n()
  )%>%
  filter(count_total > 1000)

# # plotting the times on x and y with the equivalency line plotted - mean
# ggplot(data=meen, aes(x=mean_t_member, y=mean_t_casual))+
#   geom_point()+
#   geom_abline(intercept=0, slope = 1, color = "red", linetype = "dashed") +
#   labs(x = "Member Time", y = "Casual Time", title = "Starting stations Time of casuals vs members") +  # Add axis labels and title
#   theme_minimal() +
#   coord_cartesian(xlim = c(0, 1600), ylim = c(0, 1600))  # Set plot limits

# plotting the times on x and y with the equivalency line plotted - median
ggplot(data=meed, aes(x=med_t_member, y=med_t_casual, fill=count_dif))+
  geom_point(shape = 21, size = 4)+ # Use shape 21 for filled circles
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +  # Specify the color gradient
  geom_abline(intercept=0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Member Time", y = "Casual Time", title = "Starting stations Time of casuals vs members") +  # Add axis labels and title
  theme_minimal() +
  coord_cartesian(xlim = c(0, 1600), ylim = c(0, 1600))  # Set plot limits


# seeing the distribution of ride times to determine if mean or median would be better

# histogram of time
ggplot(data=beep, aes(x=ride_time, fill=member_casual)) + 
  geom_histogram(binwidth=3000, position="dodge")+
  scale_y_continuous(trans='log10')+ #use log scale to see the lower counts easier
  labs(x = "Ride Duration (s)", y = "Count (log)", title = "Number of rides per duration")


#histogram for distance
ggplot(data=beep, aes(x=dist_total, fill=member_casual)) + 
  geom_histogram(binwidth=0.01, position="dodge")+
#  scale_y_continuous(trans='log10')+ #use log scale to see the lower counts easier
  labs(x = "lat/longitude difference", y = "Count", title = "Number of rides per distance")+
  coord_cartesian(xlim = c(0, 0.2))  # Set plot limits
# Change the width of bins
# Change colors

