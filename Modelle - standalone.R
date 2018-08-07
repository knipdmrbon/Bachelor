
############################################################################################################
##################### Model Comparison ###################################################################
############################################################################################################

# Table of Results
# Model Types
model_types = cbind(rep(c("Logistic Regression", 
                          "SVM"), 
                        each = 2))

# Model Names
model_names = cbind(rep(c("Model 1: Spam", 
                          "Model 2: Transformed"), 
                        times = 2))

# AUC, Train
model.train.auc = rbind(model1.train.auc, 
                        model2.train.auc, 
                        svm.model1.train.auc, 
                        svm.model2.train.auc
)

# Accuracy, Train
model.train.acc = rbind(model1.train.confm$overall[1], 
                        model2.train.confm$overall[1],
                        svm.model1.train.confm$overall[1], 
                        svm.model2.train.confm$overall[1]
)

# Accuracy, Test
model.test.acc = rbind(model1.test.confm$overall[1], 
                       model2.test.confm$overall[1],
                       svm.model1.test.confm$overall[1], 
                       svm.model2.test.confm$overall[1])

# Data Frame
models = data.frame(model_types, 
                    model_names, 
                    model.train.auc, 
                    model.train.acc, 
                    model.test.acc)

rownames(models) = 1:nrow(models)
colnames(models) = c("Model Type", 
                     "Model Name", 
                     "Train: AUC", 
                     "Train: Accuracy", 
                     "Test: Accuracy")