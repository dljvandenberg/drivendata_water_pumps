# Backlog

#. Prioritize backlog
#. Increase data quality:
    a. Check ratio of NAs, unknowns, zeros, wpt_name=="none", near zeros in long/latitude, date_recorded (and document reasons to exclude)
    a. Replace incorrect zeros by NAs? (and write down assumptions)
    a. Test imputation of missing values (during preProc="knnImpute"; also test on validation/test sets)
    a. Check for zero variance for specific factor levels of a specific variable when using small training set (can cause zero variance error during PCA preprocess step)
#. Troubleshoot memory issues
    a. Read more info
    a. Find limitations
    a. Don't use subvillage and wpt_name as predictors (too much memory usage)
    a. Remove unused dataframes to clear up memory (df.train.raw)?
#. Refactoring:
    a. Put code in functions
#. Improve model accuracy
    a. Use p=.75 in splitting training/validating sets
    a. Include more predictors
    a. preProcess=c("pca") -> gives zero variance error ('cannot rescale a constant/zero column to unit variance')
    a. trControl=trainControl(method="cv", number=5)
    a. method="gbm"
#. Create Shiny application based on generated model
    a. Input: predictor variables (drop down lists) for specific water pump
    a. Output: probability of status_group (type="prob") and most likely status_group (type="raw")



# Assumptions

* ..