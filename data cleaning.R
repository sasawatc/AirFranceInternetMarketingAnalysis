#################################
#Import Data
#################################
library(readr)
air_france_doubleclick <- read_csv("data/processed/air_france_doubleclick.csv", 
                                   col_types = cols(`Keyword ID` = col_character()))
View(air_france_doubleclick)

################################
#data cleaning
################################
#check structure 
data<-air_france_doubleclick
str(data)

#check identical variables and duplicated columns
library(sqldf)
sqldf("SELECT distinct data.`Keyword Type`
      FROM data")
# Keyword Type only has one output: "Unassigned". Not useful to our analysis.

identical(data[['Total Cost']],data[['Click Charges']])
# TRUE

# drop two columns
data<-within(data, rm("Keyword Type", "Click Charges"))

#check for NAs
colSums(is.na.data.frame(data))
which(is.na(data$`Publisher ID`))
data<-data[-c(4511),]
data$`Bid Strategy`[is.na(data$`Bid Strategy`)] <- "No Strategy"

str(data)
data$`Search Engine Bid`<- as.numeric(gsub('[$,]', '',data$`Search Engine Bid`))
data$`Avg. Cost per Click`<- as.numeric(gsub('[$,]', '',data$`Avg. Cost per Click`))
data$`Total Cost/ Trans`<- as.numeric(gsub('[$,]', '',data$`Total Cost/ Trans`))
data$`Amount`<- as.numeric(gsub('[$,]', '',data$`Amount`))
data$`Total Cost`<- as.numeric(gsub('[$,]', '',data$`Total Cost`))
data$`Engine Click Thru %`<- as.numeric(gsub('[%,]', '',data$`Engine Click Thru %`))
data$`Trans. Conv. %`<- as.numeric(gsub('[%,]', '',data$`Trans. Conv. %`))

write.csv(data, file="clean data.csv")
