% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runCrossValidation.R
\name{runCrossValidation}
\alias{runCrossValidation}
\title{Run Cross-Validation for A List of Algoirthms with Peak Quality Metric Feature Sets}
\usage{
runCrossValidation(
  trainData,
  k,
  repNum,
  rand.seed = NULL,
  models = "all",
  metricSet = "M11"
)
}
\arguments{
\item{trainData}{dataframe. Rows should correspond to peaks, columns should include peak quality metrics and class labels only.}

\item{k}{integer. Number of folds to be used in cross-validation}

\item{repNum}{integer. Number of cross-validation rounds to perform}

\item{rand.seed}{integer. State in which to set the random number generator}

\item{models}{character string or vector. Specifies the classification algorithms to be trained from the eight available:
DecisionTree, LogisiticRegression, NaiveBayes, RandomForest, SVM_Linear, AdaBoost, NeuralNetwork, and
ModelAveragedNeuralNetwork. "all" specifies the use of all models. Default is "all".}

\item{metricSet}{The metric set(s) to be run with the selected model(s). Select from the following: M4, M7, and M11. Use c()
to select multiple metrics. "all" specifics the use of all metrics. Default is "M11".}
}
\value{
a list of up to 8 trained models
}
\description{
Wrapper function for running cross-validation on up to 8 classification algorithms using one or more of the three available
metrics sets.
}
\examples{
# train classification algorithms
\donttest{models <- trainClassifiers(trainData=pqMetrics_development, k=5, repNum=10,
 rand.seed = 453, models="DecisionTree")}

}
