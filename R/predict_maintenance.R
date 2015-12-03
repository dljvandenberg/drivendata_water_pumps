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

# Read data sets and explicitly set classes (because of incorrect automatic factors and date)
df.train.labels <- read.csv("training_set_labels.csv", na.strings=c(""),
                            colClasses=c("factor", "factor"))
df.train.values <- read.csv("training_set_values.csv", na.strings=c(""),
                            colClasses=c("factor", "numeric", "POSIXct", "factor", "integer", "factor", "numeric", "numeric", "factor", "integer", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "integer", "factor", "factor", "factor", "factor", "factor", "integer", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor"))
df.test.values <- read.csv("test_set_values.csv", na.strings=c(""),
                           colClasses=c("factor", "numeric", "POSIXct", "factor", "integer", "factor", "numeric", "numeric", "factor", "integer", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "integer", "factor", "factor", "factor", "factor", "factor", "integer", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor"))

# Replace 0 by NA for certain variables (based on our assumption that these 0 values are incorrect..)
df.train.values$construction_year[df.train.values$construction_year==0] <- NA
df.train.values$gps_height[df.train.values$gps_height==0] <- NA
df.train.values$num_private[df.train.values$num_private==0] <- NA
df.train.values$amount_tsh[df.train.values$amount_tsh==0] <- NA
df.train.values$population[df.train.values$population==0] <- NA
df.test.values$construction_year[df.test.values$construction_year==0] <- NA
df.test.values$gps_height[df.test.values$gps_height==0] <- NA
df.test.values$num_private[df.test.values$num_private==0] <- NA
df.test.values$amount_tsh[df.test.values$amount_tsh==0] <- NA
df.test.values$population[df.test.values$population==0] <- NA

# Note: there are many pumps with wpt_name=="none". Left unchanged.

