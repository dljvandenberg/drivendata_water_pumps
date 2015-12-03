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

# Replace 0 by NA for certain variables (based on our assumption that these 0 values are incorrect..)
df.train.values$construction_year[df.train.values$construction_year==0] <- NA
df.train.values$gps_height[df.train.values$gps_height==0] <- NA
df.train.values$num_private[df.train.values$num_private==0] <- NA
df.train.values$amount_tsh[df.train.values$amount_tsh==0] <- NA
df.train.values$population[df.train.values$population==0] <- NA
df.test$construction_year[df.test$construction_year==0] <- NA
df.test$gps_height[df.test$gps_height==0] <- NA
df.test$num_private[df.test$num_private==0] <- NA
df.test$amount_tsh[df.test$amount_tsh==0] <- NA
df.test$population[df.test$population==0] <- NA

# Note: there are many pumps with wpt_name=="none". Left unchanged.

# Merge training values and labels and remove obsolete data frames
df.train <- merge(df.train.values, df.train.labels, by="id")
rm(df.train.values, df.train.labels)


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


## MACHINE LEARNING

# TODO_later: rename df.training (to prevent confusion with df.train)

# Divide into training, testing and predicting set
set.seed(1234)
m.train <- createDataPartition(df.train$status_group, p=.75, list = FALSE)
df.training <- df.train[m.train,]
df.validating <- df.train[-m.train,]

# Train
model.rpart <- train(factor(status_group) ~ ., data=df.training, method="rpart")
model.rf <- train(factor(status_group) ~ ., data=df.training, method="rf")
model.rf.2 <- train(factor(status_group) ~ ., data=df.training, method="rf", trControl=trainControl(method="cv", number=10))
model.gbm <- train(factor(status_group) ~ ., data=df.training, method="gbm")

# Select model
model.selected <- model.rpart

# Model details
model.selected
model.selected$finalModel

# Tree model visualization
library(rattle)
fancyRpartPlot(model.selected$finalModel)

# Confusion table and accuracy for validating set
list.validating.predictions <- predict(model.selected, newdata=df.validating)
table(df.validating$status_group, list.validating.predictions)
sum(df.validating$status_group == list.validating.predictions) / length(df.validating$status_group)

# Most important variables
varImp(model.selected)

# Save/load model to/from file
saveRDS(model.selected, "model_rpart.rds")
# model <- readRDS("model_rf.rds")

