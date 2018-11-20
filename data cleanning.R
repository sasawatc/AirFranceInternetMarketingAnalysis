#################################
#Import Data
#################################
library(readr)
air_france_doubleclick <- read_csv("data/processed/air_france_doubleclick.csv", 
                                   col_types = cols(X24 = col_skip(), X25 = col_skip()))
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

identical(data[['Total Cost']],data[['Click Charges']])

#drop two columns
data<-within(data, rm("Keyword Type", "Click Charges"))

#check NA
colSums(is.na.data.frame(data))
which(is.na(data$`Publisher ID`))
data<-data[-c(4511),]
data$`Bid Strategy`[is.na(data$`Bid Strategy`)] <- "No Strategy"

write.csv(data, file="clean data.csv")
