library(MASS)

#########################
##Regression of profit group
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


both <- stepAIC(full.model, trace = FALSE)
summary(both)



#########################
##Regression of ROI
#########################
ROI_no_id <- data.frame(data)
for (ROI in 1:nrow(data)){
  if (data$ROI[ROI] > 0 ){
    ROI_no_id$Target_ROI[ROI] <- 1 
  } # closing if statement
  else if (data$ROI[ROI] <= 0){
    ROI_no_id$Target_ROI[ROI] <- 0
  }# closing else if statement
  else {
    ROI_no_id$Target_ROI[ROI] <- 1000 # large number to attract eyes
  } # closing else statement
} # close for loop
ROI_no_id$Keyword.ID <- NULL # data can not be logged
ROI_no_id$Profit <- NULL # we added in, should be not include in the model
ROI_no_id$ROI <- NULL # we added in, should be not include in the model
ROI_no_id$Profit.Group <- NULL # we added in, should be not include in the model
ROI_no_id$Profit.Trans <- NULL # we added in, should be not include in the model
ROI_no_id$Amount.Booking <- NULL # we added in, should be not include in the model
ROI_no_id$Target <- NULL # we added in, should be not include in the model
ROI_no_id$ROA <- NULL # we added in, should be not include in the model
ROI_no_id$Keyword <- NULL # too many observations
ROI_no_id$Keyword.Group <- NULL # too many observations
ROI_no_id$Publisher.ID <- NULL # not relevant
ROI_no_id$Status <- NULL # not relevant

# model
full_ROI <- glm(Target_ROI ~ ., data = ROI_no_id, family = "binomial")
# coef(full.model)
summary(full_ROI)
# Select the most contributive variables
both_ROI <- stepAIC(full_ROI, trace = FALSE)
summary(both_ROI)

######################
# ROA
######################
ROA_no_id <- data.frame(data)
for (ROA in 1:nrow(ROA_no_id)){
  if (ROA_no_id$ROA[ROA] > 0 ){
    ROA_no_id$Target_ROA[ROA] <- 1 
  } # closing if statement
  else if (ROA_no_id$ROA[ROA] <= 0){
    ROA_no_id$Target_ROA[ROA] <- 0
  }# closing else if statement
  else {
    ROA_no_id$Target_ROA[ROA] <- 1000 # large number to attract eyes
  } # closing else statement
} # close for loop
ROA_no_id$Keyword.ID <- NULL # data can not be logged
ROA_no_id$Profit <- NULL # we added in, should be not include in the model
ROA_no_id$ROI <- NULL # we added in, should be not include in the model
ROA_no_id$Profit.Group <- NULL # we added in, should be not include in the model
ROA_no_id$Profit.Trans <- NULL # we added in, should be not include in the model
ROA_no_id$Amount.Booking <- NULL # we added in, should be not include in the model
ROA_no_id$Target <- NULL # we added in, should be not include in the model
ROA_no_id$ROA <- NULL # we added in, should be not include in the model
ROA_no_id$Keyword <- NULL # too many observations
ROA_no_id$Keyword.Group <- NULL # too many observations
ROA_no_id$Publisher.ID <- NULL # not relevant
ROA_no_id$Status <- NULL # not relevant

full_ROA <- glm(Target_ROA~., data = ROA_no_id, family = "binomial")

both_ROA <- stepAIC(full_ROA, trace = FALSE)
summary(both_ROA)