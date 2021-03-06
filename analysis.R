library(ggplot2)
library(dplyr)
library(sqldf)

colSums(is.na(data))
# when I call this, there are no NAs anywhere
# but when you manually sort Match Type column, there are NAs there
levels(data$`Match Type`)
# N/A is a match type (Not Applicable), all from Google (US and Global). Consider deleting.
which(is.na(data$`Publisher ID`))
# Seems like all rows containing No Strategy (originally NAs) & N/A as match type are internal searches from Google
# Just delete the N/A rows and data should be clean

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



summary(high_ROA_limit$ROA)
#############################
#Grouping method1 : Publisher
#############################
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
ggplot(high_ROA, aes(x = `Total Volume of Bookings`, y = ROA, color = `Match Type`)) + 
  geom_point(size = 4)
# Match Type: Standard

# ROA vs Total cost by Match Type
ggplot(high_ROA, aes(x= `Total Cost`, y = ROA, color = `Match Type`)) +
  geom_point(size = 4)
# Match Type: Advanced

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

# ROI vs Total cost by Keyword Group
ggplot(high_ROI_limit, aes(x = `Total Cost`, y = ROI, color = `Keyword Group`)) +
  geom_jitter(size = 4)
# keyword group: 

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

high_ROA_limit <- data_limit[data_limit$Profit >= 0,]
ggplot(high_ROA_limit, aes(x=ROA)) + 
  geom_histogram()

# ROA vs Total Volume of Bookings by Publisher Name
ggplot(high_ROA_limit, aes(x = `Total Volume of Bookings`, y = ROA, color = `Publisher Name`)) +
  geom_point(size = 4)

highlight <- high_ROA_limit[high_ROA_limit$ROA > 300 | high_ROA_limit$`Total Volume of Bookings` > 300,]
greyout <- setdiff(high_ROA_limit, highlight)

ggplot() +
  geom_point(data=greyout,
             mapping = aes(greyout$`Total Volume of Bookings`, greyout$ROA), color = "grey", size = 4) +
  geom_point(highlight, 
             mapping = aes(highlight$`Total Volume of Bookings`, highlight$ROA, color =`Publisher Name`), size =4) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black")) +
  ggtitle("Publisher Success") +
  xlab("Total Volume of Bookings") +
  ylab("ROA(%)") +
  labs(color = "Publisher Name")
# Publisher name: Yahoo-US, Overture-Global, Google-US

# ROA vs Total Volume of Bookings by Keyword Group
ggplot(high_ROA_limit, aes(x = `Total Volume of Bookings`, y = ROA, color = `Keyword Group`)) + 
  geom_point(size = 4)
# keyword group: Florence

# ROA vs Total Volume of Bookings by Match type
ggplot(high_ROA_limit, aes(x = `Total Volume of Bookings`, y = ROA, color = `Match Type`)) + 
  geom_point(size = 4)

# ROA vs Total cost by Keyword Group
ggplot(high_ROA_limit, aes(x = `Total Cost`, y = ROA, color = `Keyword Group`)) +
  geom_point(size = 4)
# keyword group: Florence

# ROA vs Total volume of Bookings by Match Type
ggplot(high_ROA_limit, aes(x = `Total Volume of Bookings`, y = ROA, color = `Match Type`)) + 
  geom_point(size = 4)
# Match Type: Advanced
ggfunc <- function(greydata, gx, gy, hightlightdata, hx, hy, title, xlab, ylab, labs){
  ggplot() +
    geom_point(data=greydata,
               mapping = aes(greydata$gx, greydata$gy), color = "grey", size = 4) +
    geom_point(highlight, 
               mapping = aes(hightlightdata$hx, hightlightdata$hy, color =`Publisher Name`), size =4) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(color = "black")) +
    ggtitle(title) +
    xlab(xlab) +
    ylab(ylab) +
    labs(color = labs)
} # closing ggfunc

ggplot(high_ROA_limit, aes(x = `Total Volume of Bookings`, y = ROA, color = Keyword)) +
  geom_point(size = 4)
goodkeyword <- high_ROA_limit[high_ROA_limit$`Total Volume of Bookings` > 190 | high_ROA_limit$ROA > 300,]

# ROA vs Total cost by Match Type
ggplot(high_ROA_limit, aes(x= `Total Cost`, y = ROA, color = `Match Type`)) +
  geom_point(size = 4)
# Match Type: Advanced

# ROA vs Total cost by publisher 
ggplot(data_limit, aes(x= `Total Cost`, y = ROA, color = `Publisher Name`)) + 
  geom_point(size = 4)
# Publisher Name: Overture


ggplot(high_ROA_limit, aes(x = `Trans Conv Percent`, y = ROA, color = `Match Type`)) +
  geom_point(size = 4)

standard_advanced <- high_ROA_limit[high_ROA_limit$`Match Type` == "Standard" |high_ROA_limit$`Match Type` == "Advanced",]
no_standard <- high_ROA_limit[high_ROA_limit$`Match Type` != "Standard" & high_ROA_limit$`Match Type` != "Advanced",]

ggplot() +
  geom_point(data=no_standard,
             mapping = aes(no_standard$`Trans Conv Percent`, no_standard$ROA), color = "grey", size = 4) +
  geom_point(standard_advanced, 
             mapping = aes(standard_advanced$`Trans Conv Percent`, standard_advanced$ROA, color =`Match Type`), size =4) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black")) +
  ggtitle("Match Type Performance \nROA") +
  xlab("Transaction Conversion Rate") +
  ylab("ROA(%)") +
  labs(color = "Match Type")

ggplot(high_ROA_limit, aes(x = `Trans Conv Percent`, y = `Total Cost`, color = `Match Type`)) +
  geom_point(size = 4)

ggplot() +
  geom_point(no_standard,
             mapping = aes(no_standard$`Trans Conv Percent`, no_standard$`Total Cost`), color = "grey", size = 4) +
  geom_point(standard_advanced, 
             mapping = aes(standard_advanced$`Trans Conv Percent`, standard_advanced$`Total Cost`, color =`Match Type`), size =4) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black")) +
  ggtitle("Match Type Performance \nTotal Cost") +
  xlab("Transaction Conversion Rate") +
  ylab("Total Cost($)") +
  labs(color = "Match Type")

ggplot(high_ROA_limit, aes(x = `Trans Conv Percent`, y = `Total Cost`, color = `Publisher Name`)) +
  geom_point(size = 4)

ggplot(high_ROA_limit, aes(x = `Publisher Name`, y = `Trans Conv Percent`, color = `Total Cost`)) +
  geom_boxplot() +
  geom_point() +
  scale_color_gradient(low = 'blue', high = 'red') +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black")) +
  ggtitle("Publisher Performance") +
  xlab("") +
  ylab("Transaction Conversion Rate(%)") +
  labs(color = "Total Cost")


ggplot(high_ROA_limit, aes(x = `Match Type`, y = ROA)) +
  geom_point(size = 4)




ggplot(high_ROA_limit, aes(x = `Trans Conv Percent`, y = ROA, color = `Bid Strategy`)) +
  geom_point(size =4)

ggplot(high_ROA_limit, aes(x = `Trans Conv Percent`, y = `Total Cost`, color = `Bid Strategy`)) +
  geom_point(size =4)

no_strategy <- high_ROA_limit[high_ROA_limit$`Bid Strategy` == "No Strategy",]
strategy <- high_ROA_limit[high_ROA_limit$`Bid Strategy` != "No Strategy",]

ggplot() +
  geom_point(strategy,
             mapping = aes(strategy$`Trans Conv Percent`, strategy$`Total Cost`), color = "grey", size = 4) +
  geom_point(no_strategy, 
             mapping = aes(no_strategy$`Trans Conv Percent`, no_strategy$`Total Cost`, color =`Bid Strategy`), size =4) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black")) +
  ggtitle("Bid Strategy Performance \nTotal Cost") +
  xlab("Transaction Conversion Rate") +
  ylab("Total Cost($)") +
  labs(color = "Bid Strategy")





ggplot(high_ROA_limit, aes(x = `Bid Strategy`, y = `Trans Conv Percent`)) +
  geom_violin() +
  geom_jitter()

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

######################
# Heatmap
######################

library(purrr)
library(tidyr)


data.num <- data %>%
  keep(negate(is.character)) %>%
  map_df(function(x) if(is.integer(x)) as.numeric(x) else x) %>%
  FactorToNum


library(ggcorrplot)
corr <- round(cor(data.num), 1)

ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)


#######################
##Keywords ranking
#######################
hrl <- high_ROA_limit %>%
  group_by(Keyword) %>%
  summarise(tcr=median(`Trans Conv Percent`))

ggplot(hrl, aes(reorder(Keyword,tcr), log(tcr), fill = tcr)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "gray", high = '#e03e32') +
  coord_flip() +
  labs(title = "What keywords should AirFrance care about?",
       x = "") +
  theme_gray()


#top 50
hrl <- high_ROA_limit %>%
  group_by(Keyword) %>%
  summarise(tcr=median(`Trans Conv Percent`))



tcrtop50 <- top_n(hrl, 50, tcr)


ggplot(tcrtop50, aes(reorder(Keyword,tcr), log(tcr), fill = tcr)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "gray", high = "black") +
  coord_flip() +
  labs(title = "What keywords should AirFrance care about?",
       x = "") +
  theme_gray()

#top 10
library(ggplot2)
hrl <- high_ROA_limit %>%
  group_by(Keyword) %>%
  summarise(tcr=median(`Trans Conv Percent`))



tcrtop10 <- top_n(hrl, 10, tcr)


ggplot(tcrtop10, aes(reorder(Keyword,tcr), log(tcr), fill = tcr)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_gradient(low = "gray", high = '#e03e32') +
  coord_flip() +
  labs(x = "") +
  theme_gray() +
  theme(axis.text = element_text(size = 15))




#bottom 10
library(ggplot2)
hrl <- high_ROA_limit %>%
  group_by(Keyword) %>%
  summarise(tcr=median(`Trans Conv Percent`))



tcrbtm10 <- top_n(hrl, -10, tcr)


ggplot(tcrbtm10, aes(reorder(Keyword,tcr), log(tcr), fill = tcr)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_gradient(low = "gray", high = "gray") +
  coord_flip() +
  labs(x = "") +
  theme_gray() +
  theme(axis.text = element_text(size = 15))
