## Google Capstone Project - Case Study 2 (Bellabeat)

## Loading the relevant libraries 

library(tidyverse)

library(janitor) 

library(lubridate)  

library(dplyr)

library(skimr) 

library(DescTools)

library(geosphere)

library(ggplot2) 

## Reading the files 

daily_activity <- read_csv("dailyActivity_merged.csv")
daily_sleep <- read_csv("sleepDay_merged.csv")  

## Inspecting the files 
str(daily_activity)
str(daily_sleep)

## Data Cleaning 

## Removing Duplicates 

## Removing based on ID and ActivityDate 
duplicated(daily_activity[,1:2])

## No duplicates in daily_activity 

## Removing based on ID and SleepDay 

dup <- duplicated(daily_sleep[,1:2]) 

# 3 duplicates 

daily_sleep <- daily_sleep[!dup,]

## Check for any missing values 

sum(is.na(daily_activity))  

## no missing values in daily_activity 

sum(is.na(daily_sleep)) 
## no missing values in daily_sleep

## Checking for 0 values 

sum(daily_activity$TotalDistance =="0")

## Remove those with 0 for total distance 

daily_activity <- daily_activity %>%
  filter(daily_activity$TotalDistance != "0")

sum(daily_activity$TotalSteps =="0")

sum(daily_activity$Calories =="0")

sum(daily_activity$TotalMinutes =="0")

sum(daily_sleep$TotalMinutesAsleep =="0")

## Number of unique IDs 

uniqueid <- count(unique(daily_activity[,1])) 
print(paste0("no of unique IDs:", uniqueid)) 
## 33 respondents rather than 30  

uniqueids <- count(unique(daily_sleep[,1])) 
print(paste0("no of unique IDs:", uniqueids)) 
## only 24 respondents, may have missing records 

## Converting data type for both ActivityDate and SleepDay 

## Data type of both columns are character, have to be converted to date time 

daily_activity$ActivityDate <- as.POSIXct(daily_activity$ActivityDate, format = "%m/%d/%Y")

class(daily_activity$ActivityDate) 


daily_sleep$SleepDay <- as.POSIXct(daily_sleep$SleepDay, format="%m/%d/%Y %H:%M:%S")

class(daily_sleep$SleepDay)

## Creating new column DayofWeek 

daily_activity$dayofweek <- wday(daily_activity$ActivityDate, label = TRUE)

daily_sleep$dayofweek <- wday(daily_sleep$SleepDay, label = TRUE)

## Creating total minutes column 
daily_activity$TotalMinutes

daily_activity[,"TotalMinutes"] <- rowSums(daily_activity[,11:14])

## Creating total hours column

daily_activity$TotalHours 

daily_activity[,"TotalHours"] <- (daily_activity[,"TotalMinutes"])/60

daily_sleep$TotalHours 

daily_sleep[,"TotalHours"] <- (daily_sleep[,"TotalMinutesAsleep"])/60 

## Creating time taken to fall asleep column 

daily_sleep$timetakentosleep 

daily_sleep[,"timetakentosleep"] <- (daily_sleep[,"TotalTimeInBed"]) - (daily_sleep[,"TotalMinutesAsleep"])


## Data Analysis 

### Descriptive Statistics

summary(daily_activity)

#### Descriptive Statistics of total distance 

summary(daily_activity$TotalDistance)

#### Descriptive Statistics of total calories 

summary(daily_activity$Calories)

#### Descriptive Statistics of total hours of sleep 

summary(daily_sleep$TotalHours)

#### Relationship between Total Distance and Calories 

cor(daily_activity$TotalDistance, daily_activity$Calories, method = "pearson")

ggplot(daily_activity, aes(x=TotalDistance, y=Calories, color = Calories))+
  geom_point()+
  scale_colour_gradient(low = "red", high = "darkred")+
  geom_smooth(method=lm)

#### Relationship between total steps and calories 

cor(daily_activity$TotalSteps, daily_activity$Calories, method = "pearson") 


ggplot(daily_activity, aes(x=TotalSteps, y=Calories, color = Calories))+
  geom_point()+
  scale_colour_gradientn(colours=rainbow(5))+
  geom_smooth(method=lm)

#### App usage across the week 

ggplot(daily_activity, aes(x=dayofweek))+
  geom_bar(stat='count', fill = "green")  

ggplot(daily_sleep, aes(x=dayofweek))+
  geom_bar(stat='count', fill = "blue")  

#### Day of week and Average Steps 

ggplot(daily_activity, aes(x=dayofweek, y=TotalSteps))+
  geom_bar(stat='summary', fun="mean", fill = "deepskyblue3") 


#### Day of week and Average Distance 

ggplot(daily_activity, aes(x=dayofweek, y=TotalDistance))+
  geom_bar(stat='summary',fun = "mean", fill="turquoise3")

#### Day of week and Average Calories  

ggplot(daily_activity, aes(x=dayofweek, y=Calories))+ 
  geom_bar(stat='summary', fun ="mean", fill ="firebrick3")

#### Day of week and Average Sleep

ggplot(daily_sleep, aes(x=dayofweek, y=TotalHours))+
  geom_bar(stat='summary', fun="mean", fill = "lightseagreen")

#### Day of Week and time taken to fall asleep

ggplot(daily_sleep, aes(x=dayofweek, y=timetakentosleep))+
  geom_bar(stat='summary', fun="mean")

#### Percentage of minutes spent on activity 

## Creating a new dataframe with the required values and headings
minutes.df <- data.frame(time = c("VeryActiveMinutes", "FairlyActiveMinutes", "LightlyActiveMinutes", "SedentaryMinutes"),
                         Value = c(sum(daily_activity$VeryActiveMinutes), sum(daily_activity$FairlyActiveMinutes), sum(daily_activity$LightlyActiveMinutes), sum(daily_activity$SedentaryMinutes))) 


## Calculating the various percentages of minutes 
minutes.df <- minutes.df %>% 
  mutate(percentage = format(round(Value/sum(minutes.df$Value) *100), nsmall = 2)) 

minutes.df 

ggplot(minutes.df, aes(x="", y=Value, fill=time))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0)+
  geom_text(aes(label = percentage, angle = 45), position = position_stack(vjust=0.5))+
  theme_void() 

