##############################################################################################################
# This function takes two lists with the same number of elements and the columns one wants to use for the
# LDA-algorithm. One list contains training and the other test data. The return value is a list of success rates.
#
# DT.train --> list of data.talbes where each data.table contains a training set.
# DT.test  --> list of data.talbes where each data.table contains a test set.
# columns ---> columns of the data-sets which should be used for training
##############################################################################################################
learnLDA <- function(DT.train, DT.test, columns, ...){
  # generate a formula with the variables supplied by columns for the LDA - algorithm
  LDA_formula <- as.formula(c("is_spam_flag ~", paste(columns, collapse = "+")))
  # apply the LDA-algorithm to the training-data --> use all variables as predictors
  L.LDA_models <- lapply(DT.train, function(x){lda(LDA_formula, data = x)})
  # use the trained LDA-models to predict the class-label for the test-data
  L.LDA_predict <- mapply(function(X,Y) {predict(X, Y)}, X = L.LDA_models, Y = DT.test, SIMPLIFY = FALSE)
  # generate a contingency table of the predicted and true class-labels
  L.LDA_results <- mapply(function(X,Y) {table(X$is_spam_flag, Y$class)}, X = DT.test, Y = L.LDA_predict, SIMPLIFY = FALSE)
  # calculate based on the contingency table the success rate of classification
  L.LDA_Errors <- lapply(L.LDA_results, function(x){sum(diag(x))/sum(x)})
  # return the success-rates
  return(L.LDA_Errors)
}