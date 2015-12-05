### Predict Water Pump Maintenance


## PREPARE

# Libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(caret)

# Workdir
setwd("~/git/drivendata_water_pumps/data")


## IMPORT AND CLEAN

# Read data sets (labels contains status of water pumps)
# and explicitly set classes (because of incorrect automatic factors and date)
df.train.labels <- read.csv("training_set_labels.csv", na.strings=c(""),
                            colClasses=c("factor", "factor"))
df.train.values <- read.csv("training_set_values.csv", na.strings=c(""),
                            colClasses=c("factor", "numeric", "POSIXct", "factor", "integer", "factor", "numeric", "numeric", "factor", "integer", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "integer", "factor", "factor", "factor", "factor", "factor", "integer", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor"))
df.test <- read.csv("test_set_values.csv", na.strings=c(""),
                           colClasses=c("factor", "numeric", "POSIXct", "factor", "integer", "factor", "numeric", "numeric", "factor", "integer", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "integer", "factor", "factor", "factor", "factor", "factor", "integer", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor"))

# TODO_later: check assumptions about 0 values (we kept these 0 for the moment in order to prevent issues during ML)
# TODO_later: check long/latitude for zero's

# Replace 0 by NA for certain variables (based on our assumption that these 0 values are incorrect..)
#df.train.values$construction_year[df.train.values$construction_year==0] <- NA
#df.train.values$gps_height[df.train.values$gps_height==0] <- NA
#df.train.values$num_private[df.train.values$num_private==0] <- NA
#df.train.values$amount_tsh[df.train.values$amount_tsh==0] <- NA
#df.train.values$population[df.train.values$population==0] <- NA
#df.test$construction_year[df.test$construction_year==0] <- NA
#df.test$gps_height[df.test$gps_height==0] <- NA
#df.test$num_private[df.test$num_private==0] <- NA
#df.test$amount_tsh[df.test$amount_tsh==0] <- NA
#df.test$population[df.test$population==0] <- NA

# Note: there are many pumps with wpt_name=="none". We left these unchanged.

# Merge training values and labels and remove obsolete data frames
df.train <- merge(df.train.values, df.train.labels, by="id")
rm(df.train.values, df.train.labels)

# Remove near zero variance variables ("gps_height"  "num_private" "recorded_by")
list.nzv <- nearZeroVar(df.train)
df.train <- df.train[-list.nzv]

# Drop id variable before applying machine learning
df.train <- subset(df.train, select=-id)



## EXPLORE

# Count status_group categories
table(df.train$status_group)

# Exploratory plots
#with(df.train, table(status_group, region))
ggplot(df.train, aes(x=region, fill=status_group)) + geom_bar()
ggplot(df.train, aes(x=source_type, fill=status_group)) + geom_bar()
ggplot(df.train, aes(x=construction_year, fill=status_group)) + geom_bar()
ggplot(df.train, aes(x=construction_year, fill=status_group)) + geom_bar(position="fill") + ylab("ratio")
ggplot(df.train, aes(x=waterpoint_type, fill=status_group)) + geom_bar(position="fill") + ylab("ratio")
ggplot(df.train, aes(x=extraction_type_group, fill=status_group)) + geom_bar(position="fill") + ylab("ratio")
ggplot(df.train, aes(x=waterpoint_type, fill=status_group)) + geom_bar(position="fill") + ylab("ratio")


## MACHINE LEARNING

# NOTE: rpart model with predictor subvillage requires 2.8Gb, wpt_name also requires a lot
# TODO: troubleshoot error when using funder, scheme_name as predictors
# TODO_later: use p=.75
# TODO_later: rename df.training (to prevent confusion with df.train)
# TODO_later: drop wpt_name?, date_recorded?
# TODO_later: remove df.train to clear up memory

# Divide into training, testing and predicting set
set.seed(1234)
m.train <- createDataPartition(df.train$status_group, p=.75, list = FALSE)
df.training <- df.train[m.train,]
df.validating <- df.train[-m.train,]

# Remove near zero variance variables ("gps_height"  "num_private" "recorded_by")
list.nzv <- nearZeroVar(df.training)
if(length(list.nzv) > 0) {
    print(paste("Removing columns with near zero variance: ", list.nzv))
    df.training <- df.training[-list.nzv]
}

# Train
set.seed(2345)
#model.rpart <- train(factor(status_group) ~ construction_year + extraction_type_group, data=df.training, method="rpart")
model.rf <- train(factor(status_group) ~ region + quantity, data=df.training, method="rf")
model.rf.1 <- train(factor(status_group) ~ construction_year + public_meeting + scheme_management + permit + extraction_type + management + payment + water_quality + quantity + source + waterpoint_type, data=df.training, method="rf")
#model.rf.2 <- train(factor(status_group) ~ amount_tsh + date_recorded + installer + longitude + latitude + basin + region + region_code + district_code + lga + ward + population + public_meeting + scheme_management + permit + construction_year + extraction_type + extraction_type_group + extraction_type_class + management + management_group + payment + payment_type + water_quality + quality_group + quantity + quantity_group + source + source_type + source_class + waterpoint_type + waterpoint_type_group, data=df.training, method="rf")
#model.rf.3 <- train(factor(status_group) ~ ., data=df.training, method="rf", trControl=trainControl(method="cv", number=10))
#model.gbm <- train(factor(status_group) ~ ., data=df.training, method="gbm")

# Select model
model.selected <- model.rf

# Model details
model.selected
model.selected$finalModel

# Confusion table and accuracy for validating set
#list.validating.predictions <- predict(model.selected, newdata=df.validating, na.action=na.fail)
list.validating.predictions <- predict(model.selected, newdata=df.validating)
table(df.validating$status_group, list.validating.predictions)
sum(df.validating$status_group == list.validating.predictions) / length(df.validating$status_group)

# Most important variables
varImp(model.selected)

# Save/load model to/from file
saveRDS(model.selected, "model_rf.rds")
#model.selected <- readRDS("model_rf.rds")