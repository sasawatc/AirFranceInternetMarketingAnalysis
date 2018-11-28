############################
#Import Clean Data
############################

library(readr)
clean_data <- read_csv("data/processed/clean data.csv",
                       col_types = cols(X1 = col_skip()))

View(clean_data)

data<-clean_data

#############################
# Pre work
#############################
#Add additional variables
data$`Amount/Booking`<- round(data$Amount/data$`Total Volume of Bookings`)
data$`Amount/Booking`<- as.numeric(gsub('NaN', 0,data$`Amount/Booking`))
data$ Profit <- data$Amount-data$`Total Cost`
data$`Profit/Trans`<- data$`Amount/Booking`-data$`Total Cost/ Trans`
data$ROI <- data$Profit / data$`Total Cost`

summary(data$Profit)

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

summary(data)


#############################
## Data exploration
#############################
colnames(data)

# Ad Publisher Name vs. Keyword Match Type
table(data$`Publisher Name`, data$`Match Type`)
# Ad Publisher Name vs. Profit Group
table(data$`Publisher Name`, data$`Profit Group`)

# Probabilities of each Ad Publisher Name in each Profit Group
round(prop.table(table(data$`Publisher Name`, data$`Profit Group`), 2)*100)

# Keyword Match Type vs. Profit Group
table(data$`Match Type`, data$`Profit Group`)
round(prop.table(table(data$`Match Type`, data$`Profit Group`), 2)*100)

library(ggplot2)

# side-by-side barchart of Publisher by Profit Group
ggplot(data, aes(x = data$`Publisher Name`, fill = data$`Profit Group`)) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90))+
  labs(fill='Profit Group')
# Google-US has a staggering amount of negative Profit keywords

# side-by-side barchart of Match Type by Profit Group
ggplot(data, aes(x = data$`Match Type`, fill = data$`Profit Group`)) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90))+
  labs(fill='Profit Group')
# broad keywords are losing the most Profit... but also the most (relatively)

#Box plot of Profit by match type

ggplot(data, aes(x = as.factor(data$`Match Type`), y = log(data$`Profit`))) +
  geom_boxplot()+
  xlab("Match Type")+
  ylab("Profit(log)")+
  ggtitle("BoxPlot Profit by Match Type")



#Other Plot way
# ggplot(data, aes(x = data$`Publisher Name`, fill = data$`Profit Group`)) + 
#   geom_bar(position = 'fill') +
#   ylab("Profit Group")
# 
# ggplot(data, aes(x = data$`Publisher Name`, fill = data$`Profit Group`)) +
#   geom_bar()

# ggplot(data, aes(x = data$`Publisher Name` )) + 
#   geom_bar() +
#   facet_wrap(~ data$`Profit Group`)




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
round(avg(data.`Profit/Trans`)) as Profit_per_trans,
round(avg(data.`Profit`)) as Profit
FROM data
GROUP BY data.`Publisher Name`
ORDER BY Profit DESC
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
      round(avg(data.`Profit/Trans`)) as Profit_per_trans,
      round(avg(data.`Profit`)) as Profit
      FROM data
      GROUP BY data.`Bid Strategy`
      ORDER BY Profit DESC
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
      round(avg(data.`Profit/Trans`)) as Profit_per_trans,
      round(avg(data.`Profit`)) as Profit
      FROM data
      GROUP BY data.`Match Type`
      ORDER BY Profit DESC
      ")

#Exact perform best
#Compare with Match Type table, very interesting
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
      round(avg(data.`Profit/Trans`)) as Profit_per_trans,
      round(avg(data.`Profit`)) as Profit
      FROM data
      GROUP BY data.`Campaign`
      ORDER BY Profit DESC
      ")
#Air France Branded performed the best

AFB <- data[which(data$Campaign=="Air France Branded"), ]
#############################
# Multiple Ranking
#############################

#Publisher-Match-Bid Strategy,  result the same by other combination order
####### Ying please explain purpose of this code
sqldf("SELECT data.`Publisher Name`,data.`Match Type`,data.`Bid Strategy`,
round(avg(data.`Search Engine Bid`),2) as Search_Engine_Bid, 
      round(avg(data.`Engine Click Thru %`),2) as Engine_Click_Thru_perc, 
      round(avg(data.`Avg. Cost per Click`), 2) as Avg_Cost_per_Click,
      round(avg(data.`Trans. Conv. %`), 2) as Trans_Conv_perc,
      round(avg(data.`Avg. Pos.`),2) as Avg_Pos,
      round(avg(data.`Total Cost/ Trans`), 2) as Total_Cost_o_Trans,
      round(avg(data.`Amount/Booking`)) as Amount_per_booking,
      round(avg(data.`Profit/Trans`)) as Profit_per_trans,
      round(avg(data.`Profit`)) as Profit
      FROM data
      GROUP BY data.`Publisher Name`,data.`Match Type`,data.`Bid Strategy`
      ORDER BY Profit DESC
      ")
# Google - US  Exact  No Strategy  perform best

###########################
##Char to Num
##########################

char_to_num <- function(x){
  x <- as.data.frame(x)#need to make sure that this is a data frame
  n_col_x <- ncol(x) #getting the count of columns for this data frame
  for (i in 1:n_col_x){
    if(is.character(x[,i])){
      options <-c()
      options <- unique(x[,i])
      if(length(options)<20) {
        for(z in 1:length(options)){
          x[,i]<- gsub(as.character(options[z]),paste(z), x[,i])
          
        }#closing z loop
        x[,i]<-as.numeric(x[,i])
      }#closing my options if statement
    }#closing if statement
  }#closing the for loop
  return(x)
}#closing char_to_num function
my_new_df <- char_to_num(x=data[1:30,])

my_new_df




##########################
##Correlation
##########################
library(magrittr)
library(dplyr)
data %>%
  summarize(r = cor(`Total Cost`, Profit))


lm(`Total Cost`~ Profit, data = data)

#########################
##Regression
#########################

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



has_booking <- data[data['Total Volume of Bookings'] > 0,]

ggplot(has_booking, aes(x = `Total Cost`)) +
  geom_histogram()


# total cost > 600 is outlier 
outlier <- has_booking[has_booking["Total Cost"] > 600,]

no_outlier <- has_booking[has_booking["Total Cost"] <= 600,]

ggplot(no_outlier, aes(x = `Total Cost`, y = ROI, color = factor(`Publisher Name`))) +
  geom_jitter(size = 4)

ggplot(no_outlier, aes(x = `Total Cost`, y = ROI, color = factor(`Match Type`))) +
  geom_point(alpha = 0.5, size = 4)

ggplot(data, aes(x = `Match Type`, y = ROI)) + 
  geom_boxplot()

sqldf("SELECT `Publisher Name`, `Match Type`, Amount, `Total Cost`, Profit, ROI
      FROM has_booking
      ORDER BY ROI DESC")