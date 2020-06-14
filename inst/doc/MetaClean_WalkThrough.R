## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(MetaClean)
library(xcms)
library(caret)


## ----installData--------------------------------------------------------------
## UNCOMMENT THIS SECTION IF YOU WISH TO USE THE MetaCleanData DATA PACKAGE

# # install devtools if not already installed
# install.packages("devtools")

# install the data package MetaCleanData from github
# devtools::install_github("KelseyChetnik/MetaCleanData")

# load MetaCleanData library
# library(MetaCleanData)


## ----load data----------------------------------------------------------------
## UNCOMMENT THIS CODE IF YOU HAVE INSTALLED MetaCleanData

# # load the example xcms
# data("eicLabels_development")
# data("eicLabels_test")
# data("fill_development")
# data("fill_test")
# data("xs_development")
# data("xs_test")


## ----evalObj------------------------------------------------------------------
## UNCOMMENT THIS CODE IF YOU HAVE INSTALLED MetaCleanData

# call getEvalObj on development data
# eicEval_development <- getEvalObj(xs = xs_development, fill = fill_development)

# call getEvalObj on test data
# eicEval_test <- getEvalObj(xs = xs_test, fill = fill_test)


## ----PeakQualityMetrics-------------------------------------------------------

## UNCOMMENT THIS CODE IF YOU HAVE INSTALLED MetaCleanData

# # calculate peak quality metrics for development dataset
# # For 500 peaks and 89 samples, takes ~2.3 mins
# pqMetrics_development <- getPeakQualityMetrics(eicEvalData = eicEval_development, eicLabels_df = eicLabels_development)
# 
# # calculate peak quality metrics for test dataset
# # For 500 peaks and 100 samples, takes ~2.6 mins
# pqMetrics_test <- getPeakQualityMetrics(eicEvalData = eicEval_test, eicLabels_df = eicLabels_test)


## ----pqmTables----------------------------------------------------------------

## IF YOU HAVE INSTALLED MetaCleanData YOU CAN COMMENT OUT THIS CODE AND PROCEED WITH THE PEAK QUALITY METRIC TABLES GENERATED IN THE PREVIOUS SECTIONS

data("pqMetrics_development")
data("pqMetrics_test")


## ----trainClassifiers, echo=FALSE---------------------------------------------

# train classification algorithms
# For 500 peaks and 89 samples takes ~17.5 mins
#models <- trainClassifiers(trainData=pqMetrics_development, k=5, repNum=10, rand.seed = 453, models="all")



## ----getEvalMeasures----------------------------------------------------------

# calculate all seven evaluation measures for each model and each round of cross-validation
#evalMeasuresDF <- getEvaluationMeasures(models=models, k=5, repNum=10)


## ----makePlots----------------------------------------------------------------

# generate bar plots for every
#barPlots <- makeBarPlots(evalMeasuresDF, emNames="All")

#plot(barPlots[[1]]) # PASS.FScore
#plot(barPlots[[4]]) # FAIL.FScore
#plot(barPlots[[7]]) # Accuracy


## ----trainBest----------------------------------------------------------------

# best performing model for example development set, rand.seed = 453, k = 5, repNum = 10 is AdaBoost

#trainData <- pqMetrics_development[,-c(1)]
#trControl <- trainControl(method = "none", savePredictions = 'final', classProbs=TRUE)

#seed <- 453
##set.seed(seed)
#best_model <- train(Class~., trainData,
#                   method="adaboost",
#                   trControl=trControl,
#                   tuneGrid=data.frame(nIter=150, method="Adaboost.M1") # list hyperparameters optimized for the algorithm
#                   )


## ----makePredictions----------------------------------------------------------
# return prediction probabilities for test dataset
#test_predictions_prob <- predict(best_model, pqMetrics_test[,-c(1)], type="prob")
# return class predictions for test dataset
#test_predictions_class <- predict(best_model, pqMetrics_test[,-c(1)])


#test_predictions <- cbind("Probabilities"=test_predictions_prob, "Class"=test_predictions_class, "EICNO"=pqMetrics_test$EICNo)

#test_evalMeasures <- calculateEvaluationMeasures(pred=test_predictions_class, pqMetrics_test$Class)


## ----saveModel----------------------------------------------------------------

# uncomment the lines below and add path where you want to save trained model
# model_path <- ""
# model_file <- paste0(model_path, "MyModel.rds")
# saveRDS(best_model, file=model_file)


## ----loadModel----------------------------------------------------------------

## UNCOMMENT THIS CODE IF YOU HAVE INSTALLED MetaCleanData

# # load model from MetaCleanData 
# data(myModel)


## ----ModelPrections-----------------------------------------------------------

## UNCOMMENT THIS CODE IF YOU HAVE INSTALLED MetaCleanData

# myModel_predictions_prob <- predict(myModel, pqMetrics_test[,-c(1)], type="prob")
# myModel_predictions_class <- predict(myModel, pqMetrics_test[,-c(1)])
# 
# myModel_predictions <- cbind("Probabilities"=myModel_predictions_prob, "Class"=myModel_predictions_class, "EICNO"=pqMetrics_test$EICNo)
# 


