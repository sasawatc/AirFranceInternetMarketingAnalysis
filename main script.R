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

# View(clean_data)

# Deep copy dataframe
data <- data.frame(clean_data, check.names = FALSE)

# Check if the dataframe is really a different one
tracemem(data) == tracemem(clean_data)

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

summary(data$Profit)
summary(data$ROI)
summary(data$ROA)

# ROI has infinity data, remove that point.
data <- subset(data, `Keyword ID` != "43000000013971488")

colSums(is.na(data))
# when I call this, there are no NAs anywhere
# but when you manually sort Match Type column, there are NAs there
levels(data$`Match Type`)
# N/A is a match type (Not Applicable), all from Google (US and Global). Consider deleting.
which(is.na(data$`Publisher ID`))
# Seems like all rows containing No Strategy (originally NAs) & N/A as match type are internal searches from Google
# Just delete the N/A rows and data should be clean

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

summary(data)

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

summary(data$ROA)

##############################
##Outliers
##############################
outlier1 <- data[which(data$Amount!=0 & data$`Total Cost`==0),]

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
# Means per Match Type
#############################
sqldf("SELECT distinct data.`Match Type`
      FROM data")

#keep <- c("Clicks", "Amount", "Total Volume of Booking")

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

# Compute summary statistics (count, mean, sd) for Amount
# by Bid Strategy group 

levels(my_new_df$`Bid Strategy`)

group_by(my_new_df, `Bid Strategy`) %>%
  summarise(
    count = n(),
    mean = mean(Amount, na.rm = TRUE),
    sd = sd(Amount, na.rm = TRUE)
  )
# For Clicks by Bid Strategy
group_by(my_new_df, `Bid Strategy`) %>%
  summarise(
    count = n(),
    mean = mean(Clicks, na.rm = TRUE),
    sd = sd(Clicks, na.rm = TRUE)
  )

#Match type
group_by(my_new_df, `Match Type`) %>%
  summarise(
    count = n(),
    mean = mean(Clicks, na.rm = TRUE),
    sd = sd(Clicks, na.rm = TRUE)
  )

#############################
#Grouping method1 : Publisher
#############################
library(sqldf)
sqldf("SELECT data.`Publisher Name`, 
round(avg(data.`Engine Click Thru Percent`),2) as Engine_Click_Thru_perc, 
round(avg(data.`Avg Cost per Click`), 2) as Avg_Cost_per_Click,
round(avg(data.`Trans Conv Percent`), 2) as Trans_Conv_perc,
round(avg(data.`Avg Pos`),2) as Avg_Pos,
round(avg(data.`Total Cost/Trans`), 2) as Total_Cost_o_Trans,
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
round(avg(data.`Engine Click Thru Percent`),2) as Engine_Click_Thru_perc, 
      round(avg(data.`Avg Cost per Click`), 2) as Avg_Cost_per_Click,
      round(avg(data.`Trans Conv Percent`), 2) as Trans_Conv_perc,
      round(avg(data.`Avg Pos`),2) as Avg_Pos,
      round(avg(data.`Total Cost/Trans`), 2) as Total_Cost_o_Trans,
      round(avg(data.`Amount/Booking`)) as Amount_per_booking,
      round(avg(data.`Profit/Trans`)) as Profit_per_trans,
      round(avg(data.`Profit`)) as Profit
      FROM data
      GROUP BY data.`Bid Strategy`
      ORDER BY Profit DESC
      ")
#Position 1-4 Bid Strategy perform best
P1_4 <- data[which(data$`Bid Strategy`=="Position 1-4 Bid Strategy"), ]

###############################
#Grouping Method 3: Match type
###############################
#sqldf("SELECT DISTINCT data.`Match Type` from data ")
sqldf("SELECT data.`Match Type`,
round(avg(data.`Search Engine Bid`),2) as Search_Engine_Bid, 
      round(avg(data.`Engine Click Thru Percent`),2) as Engine_Click_Thru_perc, 
      round(avg(data.`Avg Cost per Click`), 2) as Avg_Cost_per_Click,
      round(avg(data.`Trans Conv Percent`), 2) as Trans_Conv_perc,
      round(avg(data.`Avg Pos`),2) as Avg_Pos,
      round(avg(data.`Total Cost/Trans`), 2) as Total_Cost_o_Trans,
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
      round(avg(data.`Engine Click Thru Percent`),2) as Engine_Click_Thru_perc, 
      round(avg(data.`Avg Cost per Click`), 2) as Avg_Cost_per_Click,
      round(avg(data.`Trans Conv Percent`), 2) as Trans_Conv_perc,
      round(avg(data.`Avg Pos`),2) as Avg_Pos,
      round(avg(data.`Total Cost/Trans`), 2) as Total_Cost_o_Trans,
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
sqldf("SELECT data.`Publisher Name`,data.`Match Type`,data.`Bid Strategy`,
round(avg(data.`Search Engine Bid`),2) as Search_Engine_Bid, 
      round(avg(data.`Engine Click Thru Percent`),2) as Engine_Click_Thru_perc, 
      round(avg(data.`Avg Cost per Click`), 2) as Avg_Cost_per_Click,
      round(avg(data.`Trans Conv Percent`), 2) as Trans_Conv_perc,
      round(avg(data.`Avg Pos`),2) as Avg_Pos,
      round(avg(data.`Total Cost/Trans`), 2) as Total_Cost_o_Trans,
      round(avg(data.`Amount/Booking`)) as Amount_per_booking,
      round(avg(data.`Profit/Trans`)) as Profit_per_trans,
      round(avg(data.`Profit`)) as Profit
      FROM data
      GROUP BY data.`Publisher Name`,data.`Match Type`,data.`Bid Strategy`
      ORDER BY Profit DESC
      ")
# Google - US  Exact  No Strategy  perform best

###########################
## Factor to Num
##########################
data[] <- lapply(data, function(x){
  if(is.factor(x)) as.numeric(x) else x
})

##########################
##Correlation
##########################
#library(magrittr)
#library(dplyr)
# #data %>%
#   summarize(r = cor(`Total Cost`, Profit))
# # so...cost and profit are not related cuz r is 0.48??
# 
# lm(`Total Cost`~ Profit, data = data)
# again, not correlated?

# Scatter Plot try
str(my_new_df)
plot(my_new_df$Impressions, my_new_df$Profit)
abline(lm(my_new_df$Profit~my_new_df$Impressions))

# Correlation Matrix

#install.packages("GGally")
library(GGally)
#ggcorr() automatically plots only numeric variables
ggcorr(data,
       label = T,
       label_alpha = 0.5,
       label_size = 2,
       layout.exp = 0.1,
       size = 1.5,
       hjust = 0.75,
       nbreaks = 6,
       angle = -45,
       legend.size = 7,
       label_round = 2)

ggpairs(my_new_df,
        columns = c("ROI", "Keyword Group", "Clicks"),
        upper = list(continuous = wrap("cor", size = 10)),
        lower = list(continuous = "smooth"))

# #########################
# ##Regression of profit group
# #########################
# 
# # Split the data into training and test set
# #install.packages("caret")
# library(caret)
# no_id <- data.frame(data)
# no_id$Keyword.ID <- NULL # data can not be logged
# no_id$Profit <- NULL # we added in, should be not include in the model
# no_id$ROI <- NULL # we added in, should be not include in the model
# no_id$Profit.Group <- NULL # we added in, should be not include in the model
# no_id$Profit.Trans <- NULL # we added in, should be not include in the model
# no_id$Amount.Booking <- NULL # we added in, should be not include in the model
# no_id$Keyword <- NULL # too many observations
# no_id$Keyword.Group <- NULL # too many observations
# no_id$Publisher.ID <- NULL # not relevant
# no_id$Status <- NULL # not relevant
# 
# set.seed(123)
# training.samples <- no_id$Target %>% 
#   createDataPartition(p = 0.8, list = FALSE)
# 
# train.data  <- no_id[training.samples, ]
# test.data <- no_id[-training.samples, ]
# 
# # model
# full.model <- glm(Target ~ ., data = no_id, family = "binomial")
# # coef(full.model)
# summary(full.model)

# Select the most contributive variables
# library(MASS)
# 
# both <- stepAIC(full.model, trace = FALSE)
# summary(both)
#############
# result
#############
# glm(formula = Target ~ Publisher.Name + Match.Type + Campaign + 
#       Bid.Strategy + Search.Engine.Bid + Clicks + Avg.Cost.per.Click + 
#       Impressions + Engine.Click.Thru.Percent + Avg.Pos + Trans.Conv.Percent + 
#       Amount + Total.Cost + Total.Volume.of.Bookings, family = "binomial", 
#     data = no_id)

# step.model <- full.model %>% stepAIC(trace = FALSE)
# coef(step.model)

# 
# #########################
# ##Regression of ROI
# #########################
# ROI_no_id <- data.frame(data)
# for (ROI in 1:nrow(data)){
#   if (data$ROI[ROI] > 0 ){
#     ROI_no_id$Target_ROI[ROI] <- 1 
#   } # closing if statement
#   else if (data$ROI[ROI] <= 0){
#     ROI_no_id$Target_ROI[ROI] <- 0
#   }# closing else if statement
#   else {
#     ROI_no_id$Target_ROI[ROI] <- 1000 # large number to attract eyes
#   } # closing else statement
# } # close for loop
# ROI_no_id$Keyword.ID <- NULL # data can not be logged
# ROI_no_id$Profit <- NULL # we added in, should be not include in the model
# ROI_no_id$ROI <- NULL # we added in, should be not include in the model
# ROI_no_id$Profit.Group <- NULL # we added in, should be not include in the model
# ROI_no_id$Profit.Trans <- NULL # we added in, should be not include in the model
# ROI_no_id$Amount.Booking <- NULL # we added in, should be not include in the model
# ROI_no_id$Target <- NULL # we added in, should be not include in the model
# ROI_no_id$ROA <- NULL # we added in, should be not include in the model
# ROI_no_id$Keyword <- NULL # too many observations
# ROI_no_id$Keyword.Group <- NULL # too many observations
# ROI_no_id$Publisher.ID <- NULL # not relevant
# ROI_no_id$Status <- NULL # not relevant
# 
# # model
# full_ROI <- glm(Target_ROI ~ ., data = ROI_no_id, family = "binomial")
# # coef(full.model)
# summary(full_ROI)

# # Select the most contributive variables
# both_ROI <- stepAIC(full_ROI, trace = FALSE)
# summary(both_ROI)

#####################
# result
#####################
# glm(formula = Target_ROI ~ Publisher.Name + Match.Type + Search.Engine.Bid + 
#      Clicks + Avg.Cost.per.Click + Impressions + Engine.Click.Thru.Percent + 
#      Trans.Conv.Percent + Total.Cost.Trans + Amount + Total.Cost + 
#      Total.Volume.of.Bookings, family = "binomial", data = ROI_no_id)


# ######################
# # ROA
# ######################
# ROA_no_id <- data.frame(data)
# for (ROA in 1:nrow(ROA_no_id)){
#   if (ROA_no_id$ROA[ROA] > 0 ){
#     ROA_no_id$Target_ROA[ROA] <- 1 
#   } # closing if statement
#   else if (ROA_no_id$ROA[ROA] <= 0){
#     ROA_no_id$Target_ROA[ROA] <- 0
#   }# closing else if statement
#   else {
#     ROA_no_id$Target_ROA[ROA] <- 1000 # large number to attract eyes
#   } # closing else statement
# } # close for loop
# ROA_no_id$Keyword.ID <- NULL # data can not be logged
# ROA_no_id$Profit <- NULL # we added in, should be not include in the model
# ROA_no_id$ROI <- NULL # we added in, should be not include in the model
# ROA_no_id$Profit.Group <- NULL # we added in, should be not include in the model
# ROA_no_id$Profit.Trans <- NULL # we added in, should be not include in the model
# ROA_no_id$Amount.Booking <- NULL # we added in, should be not include in the model
# ROA_no_id$Target <- NULL # we added in, should be not include in the model
# ROA_no_id$ROA <- NULL # we added in, should be not include in the model
# ROA_no_id$Keyword <- NULL # too many observations
# ROA_no_id$Keyword.Group <- NULL # too many observations
# ROA_no_id$Publisher.ID <- NULL # not relevant
# ROA_no_id$Status <- NULL # not relevant
# 
# full_ROA <- glm(Target_ROA~., data = ROA_no_id, family = "binomial")

# both_ROA <- stepAIC(full_ROA, trace = FALSE)
# summary(both_ROA)

######################
# plot ROI
######################
ggplot(data, aes(x=ROI)) + 
  geom_histogram()
head(data$ROI, 300)

high_ROI <- data[data$ROI >= 100,]
ggplot(high_ROI, aes(x=ROI)) + 
  geom_histogram()

# ROI vs Total Volume of Bookings by Publisher Name
ggplot(high_ROI, aes(x = `Total Volume of Bookings`, y = ROI, color = `Publisher Name`)) +
  geom_jitter(size = 4)
# Publisher name: Yahoo-US

# ROI vs Total Volume of Bookings by Keyword Group
ggplot(high_ROI, aes(x = `Total Volume of Bookings`, y = ROI, color = `Keyword Group`)) + 
  geom_jitter(size = 4)
# keyword group: Florence

# ROI vs Total cost by Keyword Group
ggplot(high_ROI, aes(x = `Total Cost`, y = ROI, color = `Keyword Group`)) +
  geom_jitter(size = 4)
# keyword group: Florence

# ROI vs Total volume of Bookings by Match Type
ggplot(high_ROI, aes(x = `Total Volume of Bookings`, y = ROI, color = `Match Type`)) + 
  geom_jitter(size = 4)
# Match Type: Advanced

# ROI vs Total cost by Match Type
ggplot(high_ROI, aes(x= `Total Cost`, y = ROI, color = `Match Type`)) +
  geom_jitter(size = 4)
# Match Type: Advanced
  

######################
# plot ROA
######################
ggplot(data, aes(x=ROA)) + 
  geom_histogram()
head(round(data$ROA), 300)

high_ROA <- data[data$ROA >= 100,]
ggplot(high_ROA, aes(x=ROA)) + 
  geom_histogram()


# ROA vs Total Volume of Bookings by Publisher Name
ggplot(high_ROA, aes(x = `Total Volume of Bookings`, y = ROA, color = `Publisher Name`)) +
  geom_point(size = 4)
# Publisher name: Yahoo-US

# ROA vs Total Volume of Bookings by Keyword Group
ggplot(high_ROA, aes(x = `Total Volume of Bookings`, y = ROA, color = `Keyword Group`)) + 
  geom_point(size = 4)
# keyword group: Florence

# ROA vs Total cost by Keyword Group
ggplot(high_ROA, aes(x = `Total Cost`, y = ROA, color = `Keyword Group`)) +
  geom_point(size = 4)
# keyword group: Florence

# ROA vs Total volume of Bookings by Match Type
ggplot(high_ROA, aes(x = `Total Volume of Bookings`, y = ROA, color = `Match Type`)) + 
  geom_point(size = 4)
# Match Type: Advanced

# ROA vs Total cost by Match Type
ggplot(high_ROA, aes(x= `Total Cost`, y = ROA, color = `Match Type`)) +
  geom_point(size = 4)
# Match Type: Advanced

#####################
# limit dataset
#####################
data_limit <- data.frame(data, check.names = FALSE)
data_limit <- data_limit[data_limit$Clicks >= median(data_limit$Clicks),]
data_limit <- data_limit[data_limit$`Trans Conv Percent` >= median(data_limit$`Trans Conv Percent`),]
data_limit <- data_limit[data_limit$Impressions >= median(data_limit$Impressions),]
data_limit <- data_limit[data_limit$Engine.Click.Thru.Percent >= median(data_limit$Engine.Click.Thru.Percent),]
summary(data_limit$ROA)
######################
# plot ROI in data_limit
######################
ggplot(data_limit, aes(x=ROI)) + 
  geom_histogram()
head(data$ROI, 300)

high_ROI_limit <- data_limit[data_limit$ROI >= 100,]
ggplot(high_ROI_limit, aes(x=ROI)) + 
  geom_histogram()

# ROI vs Total Volume of Bookings by Publisher Name
ggplot(high_ROI_limit, aes(x = `Total Volume of Bookings`, y = ROI, color = `Publisher Name`)) +
  geom_jitter(size = 4)
# Publisher name: 

# ROI vs Total Volume of Bookings by Keyword Group
ggplot(high_ROI_limit, aes(x = `Total Volume of Bookings`, y = ROI, color = `Keyword Group`)) + 
  geom_jitter(size = 4)
# keyword group: 

# ROI vs Total Volume of Bookings by Keyword
ggplot(high_ROI_limit, aes(x = `Total Volume of Bookings`, y = ROI, color = Keyword)) + 
  geom_jitter(size = 4)

# ROI vs Total cost by Keyword Group
ggplot(high_ROI_limit, aes(x = `Total Cost`, y = ROI, color = `Keyword Group`)) +
  geom_jitter(size = 4)
# keyword group: 

# ROI vs Total cost by Keyword
ggplot(high_ROI_limit, aes(x = `Total Cost`, y = ROI, color = Keyword)) +
  geom_jitter(size = 4)

# ROI vs Total volume of Bookings by Match Type
ggplot(high_ROI_limit, aes(x = `Total Volume of Bookings`, y = ROI, color = `Match Type`)) + 
  geom_jitter(size = 4)
# Match Type: 

# ROI vs Total cost by Match Type
ggplot(high_ROI_limit, aes(x= `Total Cost`, y = ROI, color = `Match Type`)) +
  geom_jitter(size = 4)
# Match Type: 


######################
# plot ROA in data_limit
######################
ggplot(data_limit, aes(x=ROA)) + 
  geom_histogram()
head(round(data$ROA), 300)

high_ROA_limit <- data_limit[data_limit$ROA >= 100,]
ggplot(high_ROA_limit, aes(x=ROA)) + 
  geom_histogram()


# ROA vs Total Volume of Bookings by Publisher Name
ggplot(high_ROA_limit, aes(x = `Total Volume of Bookings`, y = ROA, color = `Publisher Name`)) +
  geom_point(size = 4)
# Publisher name: Overture - US and overture - Global

# ROA vs Total Volume of Bookings by Keyword
ggplot(high_ROA_limit, aes(x = `Total Volume of Bookings`, y = ROA, color = Keyword)) + 
  geom_point(size = 4)
# keyword group: airfrance.com

# ROA vs Total cost by Keyword
ggplot(high_ROA_limit, aes(x = `Total Cost`, y = ROA, color = Keyword)) +
  geom_point(size = 4)
# keyword group: airplane france ticket

# ROA vs Total volume of Bookings by Match Type
ggplot(high_ROA_limit, aes(x = `Total Volume of Bookings`, y = ROA, color = `Match Type`)) + 
  geom_point(size = 4)
# Match Type: Standard

# ROA vs Total cost by Match Type
ggplot(high_ROA_limit, aes(x= `Total Cost`, y = ROA, color = `Match Type`)) +
  geom_point(size = 4)
# Match Type: Advanced



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

ggplot(data, aes(x = `Match Type`, y = ROA)) + 
  geom_jitter() +
  theme_bw()

sqldf("SELECT `Publisher Name`, `Match Type`, Amount, `Total Cost`, Profit, ROI
      FROM has_booking
      ORDER BY ROI DESC")

######################
# Optimize for ROA
######################
# objective is the mean of high performance group A or B

# Heatmap

library(purrr)
library(tidyr)


data.num <- data %>%
  keep(negate(is.character)) %>%
  map_df(function(x) if(is.integer(x)) as.numeric(x) else x) %>%
  factor_to_num



corr <- round(cor(data.num), 1)

library(ggcorrplot)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)
