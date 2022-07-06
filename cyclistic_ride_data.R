##Setting Up R Studio
library("tidyverse")
library("ggplot2")

##Setting New Working Directory
setwd("~/TestR/cyclistic_may21_may22/CSV")

##Importing CSV files
may21 <- read_csv('may21.csv')
june21 <- read_csv('june21.csv')
july21 <- read_csv('july21.csv')
august21 <- read_csv('august21.csv')
september21 <- read_csv('september21.csv')
october21 <- read_csv('october21.csv')
november21 <- read_csv('november21.csv')
december21 <- read_csv('december21.csv')
january22 <- read_csv('january22.csv')
february22 <- read_csv('february22.csv')
march22 <- read_csv('march22.csv')
april22 <- read_csv('april22.csv')
may22 <- read_csv('may22.csv')

##Merging files into seasons
fall <- rbind(september21,october21)
summer <- rbind(june21,july21,august21)
spring <- rbind(may21,march22,april22,may22)
winter <- rbind(november21,december21,january22,february22)

##ANALYSING SUMMER DATA

##Calculating Average Ride Length
  mean_ride_length_summer <- summarize(summer,mean(ride_length))
##Calculating Longest Ride
  max_ride_length_summer <- summarize(summer,max(ride_length))
##Number Of Rides 
  number_of_rides_summer <- nrow(summer)
##Percentage Of Casual Rides
  casual_rides_summer <- summer %>% filter(member_casual=="casual") %>% nrow()
  percentage_casual_summer <- (casual_rides_summer/number_of_rides_summer) * 100
##Percentage Of Member Rides
  percentage_member_summer <- 100 - percentage_casual_summer
##Calculating Average Ride Length For Casual Riders
  mean_casual_ride_length_summer <- summer %>% 
    filter(member_casual=="casual") %>% 
    summarize(mean(ride_length))
##Calculating Average Ride Length For Cyclistic Members
  mean_member_ride_length_summer <- summer %>% 
    filter(member_casual=="member") %>% 
    summarize(mean(ride_length))
##Number Of Rides/Day For Casual Riders And Cyclistic Members
  ggplot(data=summer) + geom_bar(mapping=aes(x=week_day, fill=member_casual),
                                 position = "dodge") + 
    labs(title ="Number of Rides per Day For Summer")
##Bike Type Reference For Casual Riders
  summer %>% filter(member_casual=="casual") %>% 
    ggplot() + geom_bar(mapping = aes(x=rideable_type), fill="skyblue") + 
    labs(title="Bike Preference for Casual Riders for Summer")
  
##ANALYSING SPRING DATA

##Calculating Average Ride Length
  mean_ride_length_spring <- summarize(spring,mean(ride_length))
##Calculating Longest Ride
  max_ride_length_spring <- summarize(spring,max(ride_length))
##Number Of Rides 
  number_of_rides_spring <- nrow(spring)
##Percentage Of Casual Rides
  casual_rides_spring <- spring %>% filter(member_casual=="casual") %>% nrow()
  percentage_casual_spring <- (casual_rides_spring/number_of_rides_spring) * 100
##Percentage Of Member Rides
  percentage_member_spring <- 100 - percentage_casual_spring
##Calculating Average Ride Length For Casual Riders
  mean_casual_ride_length_spring <- spring %>% 
    filter(member_casual=="casual") %>% 
    summarize(mean(ride_length))
##Calculating Average Ride Length For Cyclistic Members
  mean_member_ride_length_spring <- spring %>% 
    filter(member_casual=="member") %>% 
    summarize(mean(ride_length))
##Number Of Rides/Day For Casual Riders And Cyclistic Members
  ggplot(data=spring) + geom_bar(mapping=aes(x=week_day, fill=member_casual),
                                 position = "dodge") + 
    labs(title ="Number of Rides per Day For Spring")
##Bike Type Reference For Casual Riders
  spring %>% filter(member_casual=="casual") %>% 
    ggplot() + geom_bar(mapping = aes(x=rideable_type), fill="skyblue") + 
    labs(title="Bike Preference for Casual Riders for Spring")  


##ANALYSING FALL DATA
  
##Calculating Average Ride Length
  mean_ride_length_fall <- summarize(fall,mean(ride_length))
##Calculating Longest Ride
  max_ride_length_fall <- summarize(fall,max(ride_length))
##Number Of Rides 
  number_of_rides_fall <- nrow(fall)
##Percentage Of Casual Rides
  casual_rides_fall <- fall %>% filter(member_casual=="casual") %>% nrow()
  percentage_casual_fall <- (casual_rides_fall/number_of_rides_fall) * 100
##Percentage Of Member Rides
  percentage_member_fall <- 100 - percentage_casual_fall
##Calculating Average Ride Length For Casual Riders
  mean_casual_ride_length_fall <- fall %>% filter(member_casual=="casual") %>% 
    summarize(mean(ride_length))
##Calculating Average Ride Length For Cyclistic Members
  mean_member_ride_length_fall <- fall %>% filter(member_casual=="member") %>% 
    summarize(mean(ride_length))
##Number Of Rides/Day For Casual Riders And Cyclistic Members
  ggplot(data=fall) + geom_bar(mapping=aes(x=week_day, fill=member_casual),
                                 position = "dodge") + 
    labs(title ="Number of Rides per Day For Fall")
##Bike Type Reference For Casual Riders
  fall %>% filter(member_casual=="casual") %>% 
    ggplot() + geom_bar(mapping = aes(x=rideable_type), fill="skyblue") + 
    labs(title="Bike Preference for Casual Riders for Fall")


##ANALYSING WINTER DATA
  
##Calculating Average Ride Length
  mean_ride_length_winter <- summarize(winter,mean(ride_length))
##Calculating Longest Ride
  max_ride_length_winter <- summarize(winter,max(ride_length))
##Number Of Rides 
  number_of_rides_winter <- nrow(winter)
##Percentage Of Casual Rides
  casual_rides_winter <- winter %>% filter(member_casual=="casual") %>% nrow()
  percentage_casual_winter <- (casual_rides_winter/number_of_rides_winter) * 100
##Percentage Of Member Rides
  percentage_member_winter <- 100 - percentage_casual_winter
##Calculating Average Ride Length For Casual Riders
  mean_casual_ride_length_winter <- winter %>% 
    filter(member_casual=="casual") %>% 
    summarize(mean(ride_length))
##Calculating Average Ride Length For Cyclistic Members
  mean_member_ride_length_winter <- winter %>% 
    filter(member_casual=="member") %>% 
    summarize(mean(ride_length))
##Number Of Rides/Day For Casual Riders And Cyclistic Members
  ggplot(data=winter) + geom_bar(mapping=aes(x=week_day, fill=member_casual),
                               position = "dodge") + 
    labs(title ="Number of Rides per Day For Winter")
##Bike Type Reference For Casual Riders
  winter %>% filter(member_casual=="casual") %>% 
    ggplot() + geom_bar(mapping = aes(x=rideable_type), fill="skyblue") + 
    labs(title="Bike Preference for Casual Riders for Winter")
  

##Exporting Merged Datasets
  write.csv(spring,"spring.csv", row.names = FALSE)
  write.csv(summer, "summer.csv", row.names = FALSE)
  write.csv(fall, "fall.csv", row.names = FALSE)
  write.csv(winter, "winter.csv", row.names = FALSE)
