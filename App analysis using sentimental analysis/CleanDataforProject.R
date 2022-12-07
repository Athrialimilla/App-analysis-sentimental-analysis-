# load all the libraries required 
library(dplyr)
library(tidyverse)
library(stringr)
library(scales)
library(ggplot2)
library(stringi)
library(psych)
library(Boruta)
library(mlbench) #for data set
library(caret)
library(randomForest)
library(highcharter) 
library(lubridate)
library(xts)
#Load packages

library(scales)
#install.packages("arsenal")
library(arsenal)
library(pacman)
library(highcharter) 
#install.packages("hrbrthemes")
#library(hrbrthemes)
p_load(dplyr,tidyverse,tidyr,stringr,ggplot2,visdat,readr,skimr,hrbrthemes,plotly,treemap,forcats,GGally)
#Increase size of plots from default
options(repr.plot.width=9, repr.plot.height=8)

data <- read.csv('googleplaystore.csv',stringsAsFactors=FALSE,header=T, encoding="UTF-8") 
data <- data[data$Rating != 19, ]  
#data <- data %>% distinct(App,.keep_all=TRUE)
data.clean <- data %>%
  mutate(
    # Eliminate some characters to transform Installs to numeric
    Installs = gsub("\\+", "", as.character(Installs)),
    Installs = as.numeric(gsub(",", "", Installs)),
    Size = gsub("M", "", Size),
    # Replace cells with k to 0 since it is < 1MB
    Size = ifelse(grepl("k", Size), 0, as.numeric(Size)),
    # Transform reviews to numeric
    Reviews = as.numeric(Reviews),
    # Remove currency symbol from Price, change it to numeric
    Price = as.numeric(gsub("\\$", "", as.character(Price))),
    # Last Updated to date format
    Last.Updated = mdy(Last.Updated),
    # Replace "Varies with device" to NA since it is unknown
    Min.Android.Ver = gsub("Varies with device", NA, Android.Ver),
    # Keep only version number to 1 decimal
    Min.Android.Ver = as.numeric(substr(Min.Android.Ver, start = 1, stop = 3)),
    # Drop old Android version column
    Android.Ver = NULL
  ) %>%
  filter(
    # Two apps had type as 0 or NA, they will be removed 
    Type %in% c("Free", "Paid")
  )



data.clean$Size[is.na(data.clean$Size)]<-mean(data.clean$Size,na.rm=TRUE)
sapply(data.clean, function(x) sum(is.na(x)))

data.clean$Type<-replace(data.clean$Type,data.clean$Type=="Free" ,0) 
data.clean$Type<-replace(data.clean$Type,data.clean$Type=="Paid" ,1) 
data.clean$Type<- as.numeric(data.clean$Type)

#data.clean <- data.clean[na.omit(data.clean$Rating)]

count_category <- unique(data.clean$Category)
length(count_category)
for(i in 1:length(count_category)){
   data.clean$Category <- replace(data.clean$Category,data.clean$Category==count_category[i],i) 
} 

data.clean$Category<- as.numeric(data.clean$Category)

content.rating <- unique(data.clean$Content.Rating)
for(i in 1:length(content.rating)){
  data.clean$Content.Rating <- replace(data.clean$Content.Rating,data.clean$Content.Rating==content.rating[i],i) 
} 

data.clean$Content.Rating<- as.numeric(data.clean$Content.Rating)


#Drop Unnecessary data
data.required <- data.clean[c(2,3,4,5,6,7,8,9)]
data.target <- data.clean[c(3)]

set.seed(3456)
trainIndex <- createDataPartition(data.required$Rating, 
                                  p = .8, 
                                  list = FALSE, 
                                  times = 1)

TRAIN <- data.required[trainIndex, ]
TEST <- data.required[-trainIndex, ]

# rf_model <- randomForest(Rating ~.,
#                          data = TRAIN,
#                          ntree= 550,
#                          mtry=3,
#                          keep.forest=TRUE,
#                          importance=TRUE)
# 
# library(randomForest)
# rmodel <- lm(Rating ~ .,  data = data.required)
# summary(rmodel)


str(data.clean)

library(neuralnet)
m <- neuralnet(Rating ~ Price+Size+Installs, data = TRAIN, hidden = 5)
m
P <- compute(m, TEST)
P
P$neurons
P$net.result


