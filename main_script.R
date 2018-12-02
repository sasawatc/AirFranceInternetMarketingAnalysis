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
  
  data <- subset(data, `Match Type` != "N/A")
  
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
    if(data$ROA[row] > 1){
      data$Target[row] <- 1
    }
    else if(data$ROA[row] <= 1){
      data$Target[row] <- 0
    }
    else{
      data$Target[row] <- 1000 #1000 arbitrary large number to draw attention
    }
  }
  
  lr <- glm(Target ~ `Trans Conv Percent`, data = data, family = "binomial")
  summary(lr)
  
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
  
  
  Advanced <- data[which(data$`Match Type`=="Advanced"), ]
  
  Broad <- data[which(data$`Match Type`=="Broad"), ]
  
  Exact <- data[which(data$`Match Type`=="Exact"), ]
  
  Standard <- data[which(data$`Match Type`=="Standard"), ]
  
  ### How to show just means from summary for all 3 metrics at the same time?
  
  #Clicks
  # summary(Advanced$Clicks)
  # summary(Broad$Clicks)
  # summary(Exact$Clicks)
  # summary(Standard$Clicks)
  
  mean(Advanced$Clicks)
  mean(Broad$Clicks)
  mean(Exact$Clicks)
  mean(Standard$Clicks)
  
  #Amount
  # summary(Advanced$Amount)
  # summary(Broad$Amount)
  # summary(Exact$Amount)
  # summary(Standard$Amount)
  
  mean(Advanced$Amount)
  mean(Broad$Amount)
  mean(Exact$Amount)
  mean(Standard$Amount)
  
  #Total Volume of Bookings
  # summary(Advanced$`Total Volume of Bookings`)
  # summary(Broad$`Total Volume of Bookings`)
  # summary(Exact$`Total Volume of Bookings`)
  # summary(Standard$`Total Volume of Bookings`)
  
  mean(Advanced$`Total Volume of Bookings`)
  mean(Broad$`Total Volume of Bookings`)
  mean(Exact$`Total Volume of Bookings`)
  mean(Standard$`Total Volume of Bookings`)
