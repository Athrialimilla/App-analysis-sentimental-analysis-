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
library(pacman)
library(scales)
p_load(dplyr,tidyverse,tidyr,stringr,ggplot2,visdat,readr,skimr,hrbrthemes,plotly,treemap,forcats,GGally)
#Increase size of plots from default
options(repr.plot.width=9, repr.plot.height=8)



data <- read.csv('~/Documents/Study/Spring 2021/CS5310/Proj_10k/googleplaystore.csv',stringsAsFactors=FALSE,header=T, encoding="UTF-8") 


vis_miss(data)
data <- data %>% distinct(App,.keep_all=TRUE)
#Convert Rating of 19 to NA
data$Rating <- na_if(data$Rating,19)



#Getting the overview information of sample data

data.clean <- data %>%
  mutate(
    # Eliminate some characters to transform Installs to numeric
    Installs = gsub("\\+", "", as.character(Installs)),
    Installs = as.numeric(gsub(",", "", Installs)),
    # Eliminate M to transform Size to numeric
    Size = gsub("M", "", Size),
    # Replace cells with k to 0 since it is < 1MB
    Size = ifelse(grepl("k","",Size), Size/1000, as.numeric(Size)),
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

vis_miss(data.clean)

data.frame(colnames(data.clean)) 

data.reduced <- data.clean[c(1,2,3,4,6,7,8,9,10)]

data.target <- data.clean[c(4)]
data.train <- data.clean[c(1,2,3,6,7,8,9,10)]

str(data.reduced)
summary(data.reduced)
sapply(data.reduced, function(x) sum(is.na(x)))
data.reduced <- na.omit(data.reduced)
vis_miss(data.reduced)

hist(data.reduced$Rating)

data.reduced %>%
  count(Category, Installs) %>%
  group_by(Category) %>%
  summarize(
    TotalInstalls = sum(as.numeric(Installs))
  ) %>%
  arrange(-TotalInstalls) %>%
  hchart('scatter', hcaes(x = "Category", y = "TotalInstalls", size = "TotalInstalls", color = "Category")) %>%
  hc_add_theme(hc_theme_538()) %>%
  hc_title(text = "Most popular categories (# of installs)")



data.reduced %>% 
  group_by(Category, Type) %>%
  summarize(
    n = n()
  ) %>%
  mutate(perc = round((n /sum(n))*100)) %>%
  hchart('bar', hcaes(x = 'Category', y = 'perc', group = 'Type')) %>%
  hc_plotOptions(series=list(stacking='normal')) %>%
  hc_title(text="Percentage of Free vs Paid by Category") %>%
  hc_add_theme(hc_theme_flat())


#scales library for log values to be displayed as natural numbers
ggplot(data.reduced, aes(x=Reviews, y=Rating)) +
  scale_x_continuous(trans='log10', labels=comma) +
  geom_point(aes(col=Type)) +
  labs(title="Android App Ratings vs Number of Reviews", subtitle="Google Playstore Dataset", y="Rating from 1 to 5 stars", x="Number of Reviews") +
  theme_linedraw()

ggplot(mutate(data.reduced, Category = fct_infreq(Category))) + 
  geom_bar(aes(x = Category, fill = ..count..)) +
  coord_flip()



