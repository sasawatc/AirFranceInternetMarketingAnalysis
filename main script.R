############################
#Import Clean Data
############################

library(readr)
clean_data <- read_csv("data/processed/clean data.csv")
View(clean_data)

data<-clean_data

#############################
#Pre work
#############################
#Additional value
data$`Amount/Booking`<- round(data$Amount/data$`Total Volume of Bookings`)
data$`Amount/Booking`<- as.numeric(gsub('NaN', 0,data$`Amount/Booking`))
data$ Revenue <- data$Amount-data$`Total Cost`
data$`Revenue/Trans`<- data$`Amount/Booking`-data$`Total Cost/ Trans`

summary(data$Revenue)

#Group by Revenue
#A-Above Mean, B-between Mean and 0, C-under 0
for (i in 1:nrow(data)) {

    if(data$Revenue[i]> mean(data$Revenue)) {
    data$`Revenue Group`[i]<-'A'
  } else if(data$Revenue[i]<= mean(data$Revenue) & data$Revenue[i]>0) {
    data$`Revenue Group`[i]<-'B'
  }
  else {data$`Revenue Group`[i]<-'C'}
}

summary(data)


#############################
##Data exploratory
#############################
colnames(data)

table(data$`Publisher Name`, data$`Match Type`)


table(data$`Publisher Name`, data$`Revenue Group`)
round(prop.table(table(data$`Publisher Name`, data$`Revenue Group`), 2)*100)

table(data$`Match Type`, data$`Revenue Group`)
round(prop.table(table(data$`Match Type`, data$`Revenue Group`), 2)*100)

library(ggplot2)

# side-by-side barchart of Publisher by Revenue Group
ggplot(data, aes(x = data$`Publisher Name`, fill = data$`Revenue Group`)) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90))+
  labs(fill='Revenue Group')

# side-by-side barchart of Match Type by Revenue Group
ggplot(data, aes(x = data$`Match Type`, fill = data$`Revenue Group`)) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90))+
  labs(fill='Revenue Group')

#Box plot of revenue by match type


ggplot(data, aes(x = as.factor(data$`Match Type`), y = log(data$`Revenue`))) +
  geom_boxplot()+
  xlab("Match Type")+
  ylab("Revenue(log)")+
  ggtitle("BoxPlot Revenue by Match Type")



#Other Plot way
# ggplot(data, aes(x = data$`Publisher Name`, fill = data$`Revenue Group`)) + 
#   geom_bar(position = 'fill') +
#   ylab("Revenue Group")
# 
# ggplot(data, aes(x = data$`Publisher Name`, fill = data$`Revenue Group`)) +
#   geom_bar()

# ggplot(data, aes(x = data$`Publisher Name` )) + 
#   geom_bar() +
#   facet_wrap(~ data$`Revenue Group`)




#############################
#Grouping method1 : Publisher
#############################

library(sqldf)
sqldf("SELECT data.`Publisher Name`, 
round(avg(data.`Engine Click Thru %`),2) as Engine_Click_Thru_perc, 
round(avg(data.`Avg. Cost per Click`), 2) as Avg_Cost_per_Click,
round(avg(data.`Trans. Conv. %`), 2) as Trans_Conv_perc,
round(avg(data.`Avg. Pos.`),2) as Avg_Pos,
round(avg(data.`Total Cost/ Trans`), 2) as Total_Cost_o_Trans,
round(avg(data.`Amount/Booking`)) as Amount_per_booking,
round(avg(data.`Revenue/Trans`)) as revenue_per_trans,
round(avg(data.`Revenue`)) as revenue
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
      round(avg(data.`Revenue/Trans`)) as revenue_per_trans,
      round(avg(data.`Revenue`)) as revenue
      FROM data
      GROUP BY data.`Bid Strategy`
      ORDER BY revenue DESC
      ")
#Postiion 1-4 Bid Strategy perform best
P1_4 <- data[which(data$`Bid Strategy`=="Postiion 1-4 Bid Strategy"), ]

###############################
#Grouping Method 3: Match type
###############################
#sqldf("SELECT DISTINCT data.`Match Type` from data ")
sqldf("SELECT data.`Match Type`,
round(avg(data.`Search Engine Bid`),2) as Search_Engine_Bid, 
      round(avg(data.`Engine Click Thru %`),2) as Engine_Click_Thru_perc, 
      round(avg(data.`Avg. Cost per Click`), 2) as Avg_Cost_per_Click,
      round(avg(data.`Trans. Conv. %`), 2) as Trans_Conv_perc,
      round(avg(data.`Avg. Pos.`),2) as Avg_Pos,
      round(avg(data.`Total Cost/ Trans`), 2) as Total_Cost_o_Trans,
      round(avg(data.`Amount/Booking`)) as Amount_per_booking,
      round(avg(data.`Revenue/Trans`)) as revenue_per_trans,
      round(avg(data.`Revenue`)) as revenue
      FROM data
      GROUP BY data.`Match Type`
      ORDER BY revenue DESC
      ")

#Exact perform best
#Compare with talbe, very interesting

Exact <- data[which(data$`Match Type`=="Exact"), ]

###############################
#Grouping Method 4: Campaign
###############################
#sqldf("SELECT DISTINCT data.`Match Type` from data ")
sqldf("SELECT data.`Campaign`,
      round(avg(data.`Search Engine Bid`),2) as Search_Engine_Bid, 
      round(avg(data.`Engine Click Thru %`),2) as Engine_Click_Thru_perc, 
      round(avg(data.`Avg. Cost per Click`), 2) as Avg_Cost_per_Click,
      round(avg(data.`Trans. Conv. %`), 2) as Trans_Conv_perc,
      round(avg(data.`Avg. Pos.`),2) as Avg_Pos,
      round(avg(data.`Total Cost/ Trans`), 2) as Total_Cost_o_Trans,
      round(avg(data.`Amount/Booking`)) as Amount_per_booking,
      round(avg(data.`Revenue/Trans`)) as revenue_per_trans,
      round(avg(data.`Revenue`)) as revenue
      FROM data
      GROUP BY data.`Campaign`
      ORDER BY revenue DESC
      ")
#Air France Branded perform best

AFB <- data[which(data$Campaign=="Air France Branded"), ]
#############################
#Multipul Ranking
#############################

#Publisher-Match-Bit,  result the same by other combination order
sqldf("SELECT data.`Publisher Name`,data.`Match Type`,data.`Bid Strategy`,
round(avg(data.`Search Engine Bid`),2) as Search_Engine_Bid, 
      round(avg(data.`Engine Click Thru %`),2) as Engine_Click_Thru_perc, 
      round(avg(data.`Avg. Cost per Click`), 2) as Avg_Cost_per_Click,
      round(avg(data.`Trans. Conv. %`), 2) as Trans_Conv_perc,
      round(avg(data.`Avg. Pos.`),2) as Avg_Pos,
      round(avg(data.`Total Cost/ Trans`), 2) as Total_Cost_o_Trans,
      round(avg(data.`Amount/Booking`)) as Amount_per_booking,
      round(avg(data.`Revenue/Trans`)) as revenue_per_trans,
      round(avg(data.`Revenue`)) as revenue
      FROM data
      GROUP BY data.`Publisher Name`,data.`Match Type`,data.`Bid Strategy`
      ORDER BY revenue DESC
      ")
# Google - US  Exact  No Strategy  perform best


##########################
##Correlation
##########################




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

