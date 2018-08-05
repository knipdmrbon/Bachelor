
############################################################################################################
##################### Logistic Regression ###################################################################
############################################################################################################

# Specify fit parameters
contr = trainControl(method = "none", 
                     classProbs = TRUE)

# Model 1 
# Build model
model1 = train(is_spam_flag ~ ., 
               data = DT.train, 
               method = "glmStepAIC", direction = "backward", 
               trControl = contr)

# Summary
model1$finalModel
length(model1$finalModel$coefficients)-1

# ROC curve
# Predict on train
model1.train.pred = predict(model1, newdata = DT.train, 
                            type = "prob")[, 2]

# Create ROC curve
model1.train.roc = roc(response = DT.train$is_spam_flag, predictor = model1.train.pred)
model1.train.auc = model1.train.roc$auc[1]

# Plot ROC curve
par(pty = "s")
plot(model1.train.roc, col = "blue", main = "ROC Curve for First Model")
par(pty = "m")

# Confusion matrix
# Training
model1.train.pred = predict(model1, newdata = DT.train)
model1.train.confm = confusionMatrix(model1.train.pred, DT.train$is_spam_flag)
model1.train.confm$overall[1]

# Test
model1.test.pred = predict(model1, newdata = DT.test)
model1.test.confm = confusionMatrix(model1.test.pred, DT.test$is_spam_flag)
model1.test.confm$overall[1]

# Model 2 - transformed
# Build model
model2 = train(is_spam_flag ~ ., 
               data = DT.transformed[train_rows, ], 
               method = "glmStepAIC", direction = "backward", 
               trControl = contr)

# Summary
model2$finalModel
length(model2$finalModel$coefficients)-1

# ROC curve
# Predict on train
model2.train.pred = predict(model2, newdata = DT.transformed[train_rows, ], 
                            type = "prob")[, 2]

# Create ROC curve
model2.train.roc = roc(response = DT.transformed$is_spam_flag[train_rows], 
                       predictor = model2.train.pred)
model2.train.auc = model2.train.roc$auc[1]

# Plot ROC curve
par(pty = "s")
plot(model2.train.roc, col = "red", main = "ROC Curve for Logarithmic Model")
par(pty = "m")

# Confusion matrix
# Training
model2.train.pred = predict(model2, newdata = DT.transformed[train_rows, ])
model2.train.confm = confusionMatrix(model2.train.pred, 
                                     DT.transformed$is_spam_flag[train_rows])
model2.train.confm$overall[1]

# Test
model2.test.pred = predict(model2, newdata = DT.transformed[-train_rows, ])
model2.test.confm = confusionMatrix(model2.test.pred, 
                                    DT.transformed$is_spam_flag[-train_rows])
model2.test.confm$overall[1]
