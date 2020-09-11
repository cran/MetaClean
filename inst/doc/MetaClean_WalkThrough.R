## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
# library(MetaClean)


## ----installData--------------------------------------------------------------
## UNCOMMENT THIS SECTION IF YOU WISH TO USE THE MetaCleanData DATA PACKAGE

# # install devtools if not already installed
# install.packages("devtools")

# install the data package MetaCleanData from github
# devtools::install_github("KelseyChetnik/MetaCleanData")

# load MetaCleanData library
# library(MetaCleanData)


## ----rsd example--------------------------------------------------------------

## UNCOMMENT THIS CODE IF YOU HAVE INSTALLED GITHUB PACKAGE MetaCleanData

# # load the example input data
# # example development data
# data("group_development")
# data("covar_development")
# 
# # example test data
# data("group_test")
# data("covar_test")
# 
# peak_table_development <- peakTable(group_development)
# peak_table_development <- cbind(EICNo=1:nrow(peak_table_development), peak_table_development)
# development_rsd_names <- as.character(covar_development[covar_development$SampleType=="LQC","FileNames"])
# filtered_peak_table_development <- rsdFilter(peakTable = peak_table_development,
#                                              eicColumn = "EICNo",
#                                              rsdColumns = development_rsd_names,
#                                              rsdThreshold = 0.3)
# 
# peak_table_test <- peakTable(group_test)
# peak_table_test <- cbind(EICNo=1:nrow(peak_table_test), peak_table_test)
# test_rsd_names <- as.character(covar_test[covar_test$SampleType=="LQC","FileNames"])
# filtered_peak_table_test <- rsdFilter(peakTable = peak_table_test,
#                                              eicColumn = "EICNo",
#                                              rsdColumns = test_rsd_names,
#                                              rsdThreshold = 0.3)



## ----load data----------------------------------------------------------------
## UNCOMMENT THIS CODE IF YOU HAVE INSTALLED GITHUB PACKAGE MetaCleanData

# # load the example input data
# # example development data
# data("eic_labels_development")
# data("fill_development")
# data("xs_development")
# # example test data
# data("fill_test")
# data("xs_test")



## ----evalObj------------------------------------------------------------------
## UNCOMMENT THIS CODE IF YOU HAVE INSTALLED MetaCleanData

# # call getEvalObj on development data
# eicEval_development <- getEvalObj(xs = xs_development, fill = fill_development)
# 
# # call getEvalObj on test data
# eicEval_test <- getEvalObj(xs = xs_test, fill = fill_test)


## ----PeakQualityMetrics-------------------------------------------------------
## UNCOMMENT THIS CODE IF YOU HAVE INSTALLED MetaCleanData

# # calculate peak quality metrics for development dataset
# # For 500 peaks and 89 samples, takes ~2.3 mins
# pqm_development <- getPeakQualityMetrics(eicEvalData = eicEval_development, eicLabels_df = eic_labels_development)
# 
# # calculate peak quality metrics for test dataset
# # For 500 peaks and 100 samples, takes ~2.6 mins
# pqm_test <- getPeakQualityMetrics(eicEvalData = eicEval_test)


## ----pqmTables----------------------------------------------------------------

## IF YOU HAVE INSTALLED MetaCleanData YOU CAN COMMENT OUT THIS CODE AND PROCEED WITH THE PEAK QUALITY METRIC TABLES GENERATED IN THE PREVIOUS SECTIONS

# data("pqm_development")
# data("pqm_test")


## ----trainClassifiers, echo=FALSE---------------------------------------------

# train classification algorithms
# For 500 peaks and 89 samples takes ~17.5 mins for M11
# models <- runCrossValidation(trainData=pqm_development, 
#                              k=5, 
#                              repNum=10, 
#                              rand.seed = 512, 
#                              models="all", 
#                              metricSet = c("M4", "M7", "M11"))



## ----getEvalMeasures----------------------------------------------------------

# calculate all seven evaluation measures for each model and each round of cross-validation
# evalMeasuresDF <- getEvaluationMeasures(models=models, k=5, repNum=10)


## ----makeBarPlots-------------------------------------------------------------

# generate bar plots for every
# barPlots <- getBarPlots(evalMeasuresDF, emNames="All")
# 
# plot(barPlots$M11$Pass_FScore) # Pass_FScore
# plot(barPlots$M11$Fail_FScore) # Fail_FScore
# plot(barPlots$M11$Accuracy) # Accuracy


## ----trainBest----------------------------------------------------------------

# example of optimized hyperparameters for best performing model AdaBoost M11
# hyperparameters here are nIter = 150 and method = "Adaboost.M1"
# View(models$AdaBoost_M11$pred)

# best performing model for example development set, rand.seed = 453, k = 5, repNum = 10 is AdaBoost

# hyperparameters <- models$AdaBoost_M11$pred[,c("nIter", "method")]
# hyperparameters <- unique(hyperparameters)
# 
# metaclean_model <- trainClassifier(trainData = pqm_development,
#                                    model = "AdaBoost",
#                                    metricSet = "M11",
#                                    hyperparameters = hyperparameters)



## ----saveModel----------------------------------------------------------------

# uncomment the lines below and add path where you want to save trained model
# model_file <- ""
# saveRDS(metaclean_model, file=model_file)


## ----loadModel----------------------------------------------------------------

## UNCOMMENT THIS CODE IF YOU HAVE INSTALLED MetaCleanData

# # load model from MetaCleanData 
# data(example_model)


## ----ModelPrections-----------------------------------------------------------

## UNCOMMENT THIS CODE IF YOU HAVE INSTALLED MetaCleanData

# mc_predictions <- getPredicitons(model = example_model,
#                                  testData = pqm_test,
#                                  eicColumn = "EICNo")


