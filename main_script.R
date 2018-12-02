library(ggplot2)
library(readr)
library(dplyr)

############################
#Import Clean Data
############################
clean_data <- read_csv("data/processed/clean_data.csv",
                       col_types = cols(`Keyword ID` = col_character(),
                                        `Publisher ID` = col_factor(NULL),
                                        `Publisher Name` = col_factor(NULL),
                                        `Bid Strategy` = col_factor(NULL),
                                        `Match Type` = col_factor(NULL),
                                        `Status` = col_factor(NULL),
                                        `Search Engine Bid` = col_skip()))

# Deep copy dataframe
data <- data.frame(clean_data, check.names = FALSE)

#############################
# Pre work
#############################
#Add additional variables
data$`Amount/Booking` <- round(data$Amount / data$`Total Volume of Bookings`)
data$`Amount/Booking` <- as.numeric(gsub('NaN', 0,data$`Amount/Booking`))
data$Profit <- data$Amount-data$`Total Cost`
data$`Profit/Trans` <- data$`Amount/Booking`-data$`Total Cost/Trans`
data$ROI <- data$Profit / data$`Total Cost`
# Return On Advertising
data$ROA <- data$Amount / data$`Total Cost`


# ROI has infinity data, remove that point.
data <- subset(data, `Keyword ID` != "43000000013971488")

data <- subset(data, `Bid Strategy` != "No Strategy")

#Group by Profit
#A-Above Mean, B-between Mean and 0, C-under 0
for (i in 1:nrow(data)) {
  
  if(data$Profit[i]> mean(data$Profit)) {
    data$`Profit Group`[i]<-'A'
  } else if(data$Profit[i]<= mean(data$Profit) & data$Profit[i]>0) {
    data$`Profit Group`[i]<-'B'
  }
  else {data$`Profit Group`[i]<-'C'}
}

for(row in 1:nrow(data)){
  # add 1 column to convert both A and B in Profit Group into 1 in target, C into 0
  if(data$`Profit Group`[row] == "A" | data$`Profit Group`[row] == "B"){
    data$Target[row] <- 1
  }
  else if(data$`Profit Group`[row] == "C"){
    data$Target[row] <- 0
  }
  else{
    data$Target[row] <- 1000 #1000 arbitrary large number to draw attention
  }
}

#####################
# limit dataset
#####################
data_limit <- data.frame(data, check.names = FALSE)
data_limit <- data_limit[data_limit$Clicks >= median(data_limit$Clicks),]
data_limit <- data_limit[data_limit$`Trans Conv Percent` >= median(data_limit$`Trans Conv Percent`),]
data_limit <- data_limit[data_limit$Impressions >= median(data_limit$Impressions),]
data_limit <- data_limit[data_limit$`Engine Click Thru Percent` >= median(data_limit$`Engine Click Thru Percent`),]
data_limit$Target <- NULL


high_ROI <- data[data$ROI >= 100,]
high_ROA <- data[data$ROA >= 100,]
high_ROI_limit <- data_limit[data_limit$ROI >= 100,]
high_ROA_limit <- data_limit[data_limit$Profit >= 0,]
