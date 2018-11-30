############################
#Import Clean Data
############################

library(readr)
clean_data <- read_csv("data/processed/clean_data.csv",
                       col_types = cols(`Keyword ID` = col_character(),
                                        `Publisher ID` = col_factor(NULL),
                                        `Publisher Name` = col_factor(NULL),
                                        `Bid Strategy` = col_factor(NULL),
                                        `Match Type` = col_factor(NULL),
                                        `Status` = col_factor(NULL)))

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

summary(data$Profit)
summary(data$ROI)

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
# Summaries per Match Type
#############################
sqldf("SELECT distinct data.`Match Type`
      FROM data")

keep <- c("Clicks", "Amount", "Total Volume of Booking")

Advanced <- data[which(data$`Match Type`=="Advanced"), ]
A_df <- subset(Advanced, select= keep)

Broad <- data[which(data$`Match Type`=="Broad"), ]
B_df <- data[, keep]

Exact <- data[which(data$`Match Type`=="Exact"), ]

Standard <- data[which(data$`Match Type`=="Standard"), ]

summary(Advanced)

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
#### Keyword Group and Category should not be numeric (>20 distinct values)
#### Campaign could be numeric (if increase distinct values to (has 24 distinct values)

my_new_df

str(my_new_df)
str(data)

sqldf("SELECT distinct data.`Campaign`
      FROM data")

###########################
##Factor to Num
##########################

factor_to_num <- function(x){
  x <- as.data.frame(x)#need to make sure that this is a data frame
  n_col_x <- ncol(x) #getting the count of columns for this data frame
  for (i in 1:n_col_x){
    if(is.factor(x[,i])){
      options <-c()
      options <- unique(x[,i])
      if(length(options)<8) {
        for(z in 1:length(options)){
          x[,i]<- gsub(as.numeric(options[z]),paste(z), x[,i])
          
        }#closing z loop
        x[,i]<-as.numeric(x[,i])
      }#closing my options if statement
    }#closing if statement
  }#closing the for loop
  return(x)
}#closing factor_to_num function

num_df <- factor_to_num(x=data[1:30,])
#### help! NAs introduced by coercion

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

########## kathy's attempt
str(my_new_df)
plot(my_new_df$Impressions, my_new_df$Profit)
abline(lm(my_new_df$Profit~my_new_df$Impressions))

install.packages("GGally")
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

#########################
##Regression
#########################

# Split the data into training and test set
#install.packages("caret")
library(caret)
no_id <- data.frame(data)
no_id$Keyword.ID <- NULL # data can not be logged
no_id$Profit <- NULL # we added in, should be not include in the model
no_id$ROI <- NULL # we added in, should be not include in the model
no_id$Profit.Group <- NULL # we added in, should be not include in the model
no_id$Profit.Trans <- NULL # we added in, should be not include in the model
no_id$Amount.Booking <- NULL # we added in, should be not include in the model
no_id$Keyword <- NULL # too many observations
no_id$Keyword.Group <- NULL # too many observations
no_id$Publisher.ID <- NULL # not relevant
no_id$Status <- NULL # not relevant

set.seed(123)
training.samples <- no_id$Target %>% 
  createDataPartition(p = 0.8, list = FALSE)

train.data  <- no_id[training.samples, ]
test.data <- no_id[-training.samples, ]

# model
full.model <- glm(Target ~ ., data = no_id, family = "binomial")
# coef(full.model)
summary(full.model)

# Select the most contributive variables
library(MASS)

both <- stepAIC(full.model, trace = FALSE)
summary(both)
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
  geom_jitter() +
  theme_bw()

sqldf("SELECT `Publisher Name`, `Match Type`, Amount, `Total Cost`, Profit, ROI
      FROM has_booking
      ORDER BY ROI DESC")
