
############################################################################################################
##################### Support Vector Machine ###################################################################
############################################################################################################

# Specify fit parameters
contr1 = trainControl(method = "cv", #cross validation. 10 folds is default
                      classProbs = T)


# Model 1 - spam
# Build model
svm.model1 = train(is_spam_flag ~ ., 
                   data = DT.train, 
                   method = "svmRadialWeights", #radial kernel
                   trControl = contr1)

# Summary
svm.model1$finalModel

# ROC curve
# Predict on train
svm.model1.train.pred = predict(svm.model1, newdata = DT.train, 
                                type = "prob")[, 2]

# Create ROC curve
svm.model1.train.roc = roc(response = DT.train$is_spam_flag, 
                           predictor = svm.model1.train.pred)
svm.model1.train.auc = svm.model1.train.roc$auc[1]

# Plot ROC curve
par(pty = "s")
plot(svm.model1.train.roc, col = "blue", main = "ROC Curve for SVM First Model")
par(pty = "m")

# Confusion matrix
# Training
svm.model1.train.pred = predict(svm.model1, newdata = DT.train)
svm.model1.train.confm = confusionMatrix(svm.model1.train.pred, 
                                         DT.train$is_spam_flag)
svm.model1.train.confm$overall[1]

# Test
svm.model1.test.pred = predict(svm.model1, newdata = DT.test)
svm.model1.test.confm = confusionMatrix(svm.model1.test.pred, 
                                        DT.test$is_spam_flag)
svm.model1.test.confm$overall[1]

# Model 2 - transformed
# Build model
svm.model2 = train(is_spam_flag ~ ., 
                   data = DT.transformed[train_rows,], 
                   method = "svmRadialWeights", 
                   trControl = contr1)

# Summary
svm.model2$finalModel

# ROC curve
# Predict on train
svm.model2.train.pred = predict(svm.model2, newdata = DT.transformed[train_rows,], 
                                type = "prob")[, 2]

# Create ROC curve
svm.model2.train.roc = roc(response = DT.transformed$is_spam_flag[train_rows], 
                           predictor = svm.model2.train.pred)
svm.model2.train.auc = svm.model2.train.roc$auc[1]

# Plot ROC curve
par(pty = "s")
plot(svm.model2.train.roc, col = "red", main = "ROC Curve for Transformed SVM")
par(pty = "m")

# Confusion matrix
# Training
svm.model2.train.pred = predict(svm.model2, newdata = DT.transformed[train_rows,])
svm.model2.train.confm = confusionMatrix(svm.model2.train.pred, 
                                         DT.transformed$is_spam_flag[train_rows])
svm.model2.train.confm$overall[1]

# Test
svm.model2.test.pred = predict(svm.model2, newdata = DT.transformed[-train_rows,])
svm.model2.test.confm = confusionMatrix(svm.model2.test.pred, 
                                        DT.transformed$is_spam_flag[-train_rows])
svm.model2.test.confm$overall[1]
