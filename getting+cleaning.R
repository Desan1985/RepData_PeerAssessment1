##rr project 1

##setnew working directory - need adjustment for our own directory
setwd("/Users/martinschultze/Desktop/R-stuff/RepData_PeerAssessment1")
getwd()

##Loading and preprocessing the data

##preparing dataset
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url,destfile = "activity.zip", method = "curl")

unzip("activity.zip", exdir = ".")
df_full <- read.csv("activity.csv", sep = ",", header = TRUE)


##transforming date 
df_full$date <- as.Date(df_full$date, format="%Y-%m-%d")
str(df_full)

