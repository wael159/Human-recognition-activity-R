#-----cross validation
library("rpart")
library("rpart.plot")
install.packages("rpart.plot")
install.packages("gridExtra")
library(RColorBrewer)
library(rattle)
library("caret")
library(ggplot2)     # to plot
library(gridExtra)   # to put more
library(grid) # plot together
# The shuffled dataset is already loaded into your workspace
#merge the train and test
raw_data1=bind_rows(train_main1,test)
# Set random seed. Don't remove this line.
set.seed(1)
#shuffeling the data
n <- nrow(raw_data1)
shuffled <- raw_data1[sample(n),]

# Initialize the accs vector
accs <- rep(0,5)

for (i in 1:5) {
  # These indices indicate the interval of the test set
  indices <- (((i-1) * round((1/5)*nrow(shuffled))) + 1):((i*round((1/5) * nrow(shuffled))))
  
  # Exclude them from the train set
  train <- shuffled[-indices,]
  
  # Include them in the test set
  test <- shuffled[indices,]
  
  # A model is learned using each training set
  tree <- rpart(Activity ~ ., train, method = "class")
  
  # Make a prediction on the test set using tree
  pred=predict(tree,newdata=test,type="class")
  
  # Assign the confusion matrix to conf
  conf=table(test$Activity,pred)
  
  # Assign the accuracy of this model to the ith index in accs
  accs[i] <- sum(diag(conf))/sum(conf)
}

# Print out the mean of accs
mean(accs)

#classification by tree
tree <- rpart(Activity ~ ., train, method = "class")
#plot the tree
fancyRpartPlot(tree)
#prediction
pred_t=predict(tree, test, type = "class")

#plot the confution matrix with accuracy
cm_acc_t=confusionMatrix(test$Activity, pred_t)

#take F1,precision,recall,accuarcy
as.data.frame(cm_acc_t$byClass)%>%select(Precision,Recall,F1,'Balanced Accuracy')

cm_dt <- as.data.frame(cm_acc_t$table)
cm_d_t <-  ggplot(data = cm_dt, aes(x = Prediction , y =  Reference, fill = Freq))+
  geom_tile() +
  geom_text(aes(label = paste("",Freq)), color = 'white', size = 5) +
  theme_dark() +
  guides(fill=FALSE) 
cm_stt <-data.frame(cm_acc_g$overall[1:6])
# plotting the stats
cm_st_p2 <-  tableGrob(cm_stt)

#plotting the cm of tree with statistics 
grid.arrange(cm_d_t, cm_st_p2,nrow = 1, ncol = 2, 
             top=textGrob("Confusion Matrix and Statistics for tree classifictaion ",gp=gpar(fontsize=25,font=1)))










#-----------pruning the tree
set.seed(1)
tree <- rpart(Activity ~ ., train, method = "class", control = rpart.control(cp=0.00001))

# Draw the complex tree
fancyRpartPlot(tree)

# Prune the tree: pruned
pruned=prune(tree,cp=0.01)

# Draw pruned
fancyRpartPlot(pruned)




#-----gini 
# Set random seed. Don't remove this line.
set.seed(1)

# Train and test tree with gini criterion
tree_g <- rpart(Activity ~ ., train_main1, method = "class",parms = list(split = "gini"))
pred_g <- predict(tree_g, test, type = "class")
conf_g <- table(test$Activity, pred_g)
acc_g <- sum(diag(conf_g)) / sum(conf_g)
cm_acc_g=confusionMatrix(test$Activity, pred_g)
cm_d1 <- as.data.frame(cm_acc_g$table)
cm_d_p1 <-  ggplot(data = cm_d1, aes(x = Prediction , y =  Reference, fill = Freq))+
  geom_tile() +
  geom_text(aes(label = paste("",Freq)), color = 'white', size = 5) +
  theme_dark() +
  guides(fill=FALSE) 
cm_st1 <-data.frame(cm_acc_g$overall[1:6])
# plotting the stats
cm_st_p1 <-  tableGrob(cm_st1)
grid.arrange(cm_d_p1, cm_st_p1,nrow = 1, ncol = 2, 
             top=textGrob("Confusion Matrix and Statistics for tree classifictaion with gini criterion",gp=gpar(fontsize=25,font=1)))

# Change the first line of code to use information gain as splitting criterion
tree_i <- rpart(Activity ~ ., train_main1, method = "class", parms = list(split = "information"))
pred_i <- predict(tree_i, test, type = "class")
conf_i <- table(test$Activity, pred_i)
acc_i <- sum(diag(conf_i)) / sum(conf_i)

cm_acc_i=confusionMatrix(test$Activity, pred_i)
cm_i <- as.data.frame(cm_acc_i$table)
cm_i_p1 <-  ggplot(data = cm_i, aes(x = Prediction , y =  Reference, fill = Freq))+
  geom_tile() +
  geom_text(aes(label = paste("",Freq)), color = 'white', size = 5) +
  theme_dark() +
  guides(fill=FALSE) 
cm_st_i <-data.frame(cm_acc_i$overall[1:6])
# plotting the stats
cm_st_p2 <-  tableGrob(cm_st_i)
grid.arrange(cm_i_p1, cm_st_p2,nrow = 1, ncol = 2, 
             top=textGrob("Confusion Matrix and Statistics for tree classifictaion with information criterion",gp=gpar(fontsize=25,font=1)))



# Draw a fancy plot of both tree_g and tree_i
fancyRpartPlot(tree_i)
fancyRpartPlot(tree_g)


# Print out acc_g and acc_i
acc_g
acc_i



# Plot the loan_model with customized settings
rpart.plot(loan_model, type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE)

#-------Random Forest

# Load the randomForest package
library("randomForest")

# Build a random forest model
loan_model <- randomForest(outcome ~ ., data = loans_train)

# Compute the accuracy of the random forest
loans_test$pred <- predict(loan_model,loans_test)
mean(loans_test$outcome==loans_test$pred)







