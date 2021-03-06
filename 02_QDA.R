##############################################################################################################
# This function takes two lists with the same number of elements and the columns one wants to use for the
# QDA-algorithm. One list contains training and the other test data. The return value is a list of success rates.
#
# DT.train --> list of data.talbes where each data.table contains a training set.
# DT.test  --> list of data.talbes where each data.table contains a test set.
# columns ---> columns of the data-sets which should be used for training
##############################################################################################################
learnQDA <- function(DT.train, DT.test, columns, ...){
  # generate a formula with the variables supplied by columns for the QDA - algorithm
  QDA_formula <- as.formula(c("is_spam_flag ~", paste(columns, collapse = "+")))
  # apply the LDA-algorithm to the training-data --> use all variables as predictors
  L.QDA_models <- lapply(DT.train, function(x){qda(QDA_formula, data = x)})
  # use the trained LDA-models to predict the class-label for the test-data
  L.QDA_predict <- mapply(function(X,Y) {predict(X, Y)}, X = L.QDA_models, Y = DT.test, SIMPLIFY = FALSE)
  # generate a contingency table of the predicted and true class-labels
  L.QDA_results <- mapply(function(X,Y) {table(X$is_spam_flag, Y$class)}, X = DT.test, Y = L.QDA_predict, SIMPLIFY = FALSE)
  # calculate based on the contingency table the success rate of classification
  L.QDA_Errors <- lapply(L.QDA_results, function(x){sum(diag(x))/sum(x)})
  # return the success-rates
  return(L.QDA_Errors)
}