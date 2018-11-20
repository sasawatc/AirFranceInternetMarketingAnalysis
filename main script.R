############################
#Import Clean Data
############################

library(readr)
clean_data <- read_csv("data/processed/clean data.csv")
View(clean_data)

data<-clean_data

#############################
summary(data)
#############################

#############################
#Grouping method1 : Publisher
#############################
data$`Amount/Booking`<- round(data$Amount/data$`Total Volume of Bookings`)
data$`Amount/Booking`<- as.numeric(gsub('NaN', 0,data$`Amount/Booking`))
data$`Revenue/Trans`<- data$`Amount/Booking`-data$`Total Cost/ Trans`
library(sqldf)
sqldf("SELECT data.`Publisher Name`, 
round(avg(data.`Engine Click Thru %`),2) as Engine_Click_Thru_perc, 
round(avg(data.`Avg. Cost per Click`), 2) as Avg_Cost_per_Click,
round(avg(data.`Trans. Conv. %`), 2) as Trans_Conv_perc,
round(avg(data.`Avg. Pos.`),2) as Avg_Pos,
round(avg(data.`Total Cost/ Trans`), 2) as Total_Cost_o_Trans,
round(avg(data.`Amount/Booking`)) as Amount_per_booking,
round(avg(data.`Revenue/Trans`)) as revenue
FROM data
GROUP BY data.`Publisher Name`
ORDER BY revenue DESC
                 ")
#Google_global perform best


#############################
#Grouping Method 2: Bid strategy
#############################
sqldf("SELECT data.`Bid Strategy`,
round(avg(data.`Search Engine Bid`),2) as Search_Engine_Bid, 
round(avg(data.`Engine Click Thru %`),2) as Engine_Click_Thru_perc, 
      round(avg(data.`Avg. Cost per Click`), 2) as Avg_Cost_per_Click,
      round(avg(data.`Trans. Conv. %`), 2) as Trans_Conv_perc,
      round(avg(data.`Avg. Pos.`),2) as Avg_Pos,
      round(avg(data.`Total Cost/ Trans`), 2) as Total_Cost_o_Trans,
      round(avg(data.`Amount/Booking`)) as Amount_per_booking,
      round(avg(data.`Revenue/Trans`)) as revenue
      FROM data
      GROUP BY data.`Bid Strategy`
      ORDER BY revenue DESC
      ")
#Position 1 -2 Target perform best





######################
#Might use later
######################
#Grouping
# yahoo_us <- data[which(data$`Publisher Name`=="Yahoo - US"),]
# msn_global <- data[which(data$`Publisher Name`=="MSN - Global"),]
# google_global <- data[which(data$`Publisher Name`=="Google - Global"),]
# overture_global <- data[which(data$`Publisher Name`=="Overture - Global"),]
# google_us <- data[which(data$`Publisher Name`=="Google - US"),]
# overture_us <- data[which(data$`Publisher Name`=="Overture - US"),]
# msn_us <- data[which(data$`Publisher Name`=="MSN - US"),]

