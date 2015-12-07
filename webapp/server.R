### Water Pump Maintenance Predictor web-app (server component)


## Run once (at application start)

# Libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(caret)

# Set variables
setwd("~/git/drivendata_water_pumps/webapp")
file.model <- "../models/model.rf.2_p02_region_quantity_waterpointtype_payment.rds"

# Load prediction model
model.selected <- readRDS(file.model)


## Interactive user session code

shinyServer(
    function(input, output) {
        # Retrieve df.inputdata from UI input
        df.inputdata <- reactive({
        })
        
        # Predict by applying model to df.inputdata
        l.test.predictions <- predict(model.selected, newdata=df.inputdata, type="prob", na.action=na.fail)
        
        # Render output
    }
)