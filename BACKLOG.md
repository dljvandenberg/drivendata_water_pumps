# Backlog

#. Prioritize backlog
#. Increase data quality:
    a. Check ratio of NAs, unknowns, zeros, wpt_name=="none", near zeros in long/latitude, date_recorded (and document reasons to exclude)
    a. Replace incorrect zeros by NAs? (and write down assumptions)
    a. Test imputation of missing values (during preProc="knnImpute"; also test on validation/test sets)
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
    a. PCA preprocessing
    a. trControl=trainControl(method="cv", number=5)
    a. method="gbm"
#. Create Shiny application based on generated model
    a. Input: predictor variables (drop down lists) for specific water pump
    a. Output: probability of status_group (type="prob") and most likely status_group (type="raw")


# Notes

* Variables with nonzero NA ratio:

```
 scheme_name           0.474175084
 scheme_management     0.065269360
 installer             0.061531987
 funder                0.061195286
 public_meeting        0.056127946
 permit                0.051447811
 subvillage            0.006245791
```


# Assumptions

* ..