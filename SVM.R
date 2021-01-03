library("e1071")
library("caret")
library("grid")
library(gridExtra)
svmfit1 = svm(Activity ~ ., data = train_main1, kernel = "radial", cost = 10, scale = FALSE,type = "C-classification")

#predicition for the test
prediction_test <- predict(svmfit1, test)
table(test$Activity,prediction1)
cm_svm=confusionMatrix(test$Activity,prediction1)

#plot the confusion matrix of the test
plot_con_matrixc(cm_svm)

cm_svm$overall
cm_svm$byClass

#plot the confusion matrix of the Train
prediction_train=predict(svmfit1, train_main1)
cm_svm_train=confusionMatrix(train_main1$Activity,prediction_train)
plot_con_matrixc(cm_svm_train)

