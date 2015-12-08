### Predict Water Pump Maintenance

# Convention: prefix of variable names denotes type (l.* = list, v.* = vector, m.* = matrix, df.* = dataframe, model.* = model)


## PREPARE

# Libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(caret)

# Workdir
setwd("~/git/drivendata_water_pumps")

# Constants
seed.universal=1234
filter.level.naratio=.0
filter.levels.max=30
training.set.ratio=.2


## IMPORT AND MERGE

# Read data sets (labels contains status of water pumps)
# and explicitly set classes (because of incorrect automatic factors and date)
df.train.labels <- read.csv("./data/training_set_labels.csv", na.strings=c(""),
                            colClasses=c("factor", "factor"))
df.train.values <- read.csv("./data/training_set_values.csv", na.strings=c(""),
                            colClasses=c("factor", "numeric", "POSIXct", "factor", "integer", "factor", "numeric", "numeric", "factor", "integer", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "integer", "factor", "factor", "factor", "factor", "factor", "integer", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor"))
df.test <- read.csv("./data/test_set_values.csv", na.strings=c(""),
                           colClasses=c("factor", "numeric", "POSIXct", "factor", "integer", "factor", "numeric", "numeric", "factor", "integer", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "integer", "factor", "factor", "factor", "factor", "factor", "integer", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor"))

# Merge training values and labels and remove obsolete data frames
df.train.raw <- merge(df.train.values, df.train.labels, by="id")
rm(df.train.values, df.train.labels)



## EXPLORE RAW DATA

# Check data quality (ratio of NAs)
summary(df.train.raw)
plot(as.vector(colSums(is.na(df.train.raw))/(dim(df.train.raw)[1])))

# Count status_group categories
table(df.train.raw$status_group)

# Exploratory plots
#with(df.train.raw, table(status_group, region))
ggplot(df.train.raw, aes(x=region, fill=status_group)) + geom_bar()
ggplot(df.train.raw, aes(x=source_type, fill=status_group)) + geom_bar()
ggplot(df.train.raw, aes(x=construction_year, fill=status_group)) + geom_bar()
ggplot(df.train.raw, aes(x=construction_year, fill=status_group)) + geom_bar(position="fill") + ylab("ratio")
ggplot(df.train.raw, aes(x=waterpoint_type, fill=status_group)) + geom_bar(position="fill") + ylab("ratio")
ggplot(df.train.raw, aes(x=management, fill=status_group)) + geom_bar(position="fill") + ylab("ratio")



## CLEAN

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

# Remove columns with na.ratio greater than filter.level.naratio
v.train.na.ratios <- as.vector(colSums(is.na(df.train.raw))/(dim(df.train.raw)[1]))
df.train.select <- df.train.raw[, v.train.na.ratios<=filter.level.naratio]

# Remove near zero variance variables
v.nzv <- nearZeroVar(df.train.select)
df.train.select <- df.train.select[-v.nzv]

# Remove id variable before applying machine learning (likely correlation by experiment design)
df.train.select <- subset(df.train.select, select=-id)

# Remove factor variables with more levels than filter.levels.max
v.too.many.factor.levels <- unlist(lapply(df.train.select, function(var) { is.factor(var) && length(levels(var))>=filter.levels.max }))
df.train.select <- df.train.select[, !v.too.many.factor.levels]

# Manually removing variables with high likelihood of becoming zero variance (for now) after sampling
df.train.select <- subset(df.train.select, select=-c(extraction_type, region_code, district_code))



## TRAIN MACHINE LEARNING MODEL

# Divide into training, testing and predicting set
set.seed(seed.universal)
m.train <- createDataPartition(df.train.select$status_group, p=training.set.ratio, list = FALSE)
df.training <- df.train.select[m.train,]
df.validating <- df.train.select[-m.train,]

# Remove near zero variance variables
v.nzv <- nearZeroVar(df.training)
if(length(v.nzv) > 0) {
    print(paste("Removing columns with near zero variance: ", v.nzv))
    df.training <- df.training[-v.nzv]
}

# Train
set.seed(seed.universal)
#model.rpart.1 <- train(factor(status_group) ~ region + quantity, data=df.training, method="rpart", preProc="knnImpute")
#model.rf.1 <- train(factor(status_group) ~ region + quantity, data=df.training, method="rf", preProc="knnImpute")
#model.rf.2 <- train(factor(status_group) ~ region + quantity + waterpoint_type + payment, data=df.training, method="rf", preProc="knnImpute")
#model.rf.3 <- train(factor(status_group) ~ region + quantity + waterpoint_type + payment + extraction_type + management + water_quality + source, data=df.training, method="rf", preProc="knnImpute")
#model.rf.4 <- train(factor(status_group) ~ region + quantity + waterpoint_type + payment + extraction_type + management + water_quality + source, data=df.training, method="rf")
model.rf.5 <- train(status_group ~ ., data=df.training, method="rf", preProcess=c("pca"), trControl = trainControl(method="cv"))

# Select model
model.selected <- model.rf.5

# Model details
model.selected
model.selected$finalModel

# Confusion table and accuracy for validating set
v.validating.predictions <- predict(model.selected, newdata=df.validating, na.action=na.fail)
table(df.validating$status_group, v.validating.predictions)
sum(df.validating$status_group == v.validating.predictions) / length(df.validating$status_group)

# Most important variables
varImp(model.selected)

# Save/load model to/from file
saveRDS(model.selected, "./models/model.rf.5_p02_pca_25vars.rds")



## PREDICT ON TEST SET AND SAVE RESULTS

#model.selected <- readRDS("./models/model.rf.5_p02_region_quantity_waterpointtype_payment_extractiontype_management_waterquality_source.rds")

# Apply model to test set
v.test.predictions <- predict(model.selected, newdata=df.test, na.action=na.fail)

# Save results as csv with variables id, status_group
df.predictions <- transform(df.test, status_group=v.test.predictions)
df.predictions <- subset(df.predictions, select=c("id", "status_group"))
write.csv(df.predictions, "./predictions/predictions.rf.5_p02_pca_25vars.csv", row.names=FALSE)
