#random forest 
#Here you will plot the OOB error as a function of the number of trees trained, and extract the final OOB error of the Random Forest model from the trained model object.
# Train a Random Forest
install.packages("randomForest")
library("randomForest")
library(caret)
library(gridExtra)   # to put more
library(grid)

set.seed(1)  # for reproducibility
activity_model <- randomForest(formula = Activity ~ ., 
                             data = train_main1)

# Print the model output                             
print(activity_model)


# Grab OOB error matrix & take a look
err <- activity_model$err.rate
head(err)

# Look at final OOB error rate (last row in err matrix)
oob_err <- err[nrow(err)
               , "OOB"]
print(oob_err)

# Plot the model trained in the previous exercise
plot(activity_model)

# Add a legend since it doesn't have one by default
legend(x = "right", 
       legend = colnames(err),
       fill = 1:ncol(err))

# Generate predicted classes using the model object
class_prediction <- predict(object = activity_model,   # model object 
                            newdata = test,  # test dataset
                            type = "class") # return classification labels


# Calculate the confusion matrix for the test set
cm_rf <- confusionMatrix(data = class_prediction,       # predicted classes
                      reference = test$Activity)  # actual classes
print(cm_rf)

#take F1,precision,recall,accuarcy
as.data.frame(cm_rf$byClass)%>%select(Precision,Recall,F1,'Balanced Accuracy')

# Compare test set accuracy to OOB accuracy
paste0("Test Accuracy: ", cm_rf$overall[1])
paste0("OOB Accuracy: ", 1 - oob_err)
plot_con_matrixc(cm_rf)
summary_confustion(cm_rf$byClass)
#---------------------------------------------------------------------plot cpnfustion matrix function
plot_con_matrixc<-function(x){

cm<- as.data.frame(x$table)
cm_st <-data.frame(x$overall[1:6]) # overall cm statstic for the LDA
colnames(cm_st)="cm overall"
# round the values

cm$Prediction=as.factor(cm$Prediction)
levels(cm$Prediction)=c("LAYING","SITTING","STANDING","WALKING","DOWN-STAIRS","UP-STAIRS")
# plotting the matrix
cm_tot <-  ggplot(data = cm, aes(x = Prediction , y =  Reference, fill = Freq))+
  geom_tile() +
  geom_text(aes(label = paste("",Freq)), color = 'white', size = 5) +
  theme_dark() +
  guides(fill=FALSE) 

# plotting the stats
cm_st<-  tableGrob(cm_st)
grid.arrange(cm_tot, cm_st,nrow = 1, ncol = 2, 
             top=textGrob("Confusion Matrix and Statistics ",gp=gpar(fontsize=25,font=1)))

}

# --------------------------------------------------------------------Execute the tuning process
set.seed(1)              
res <- tuneRF(x = subset(train_main1, select = -Activity),
              y = train_main1$Activity,
              ntreeTry = 500)

# Look at results
print(res)

# Find the mtry value that minimizes OOB Error
mtry_opt <- res[,"mtry"][which.min(res[,"OOBError"])]
print(mtry_opt)


#--dont need it--not working---------------------------------------------------------

# Establish a list of possible values for mtry, nodesize and sampsize
mtry <- seq(12, ncol(train_main1) * 0.8, 2)
nodesize <- seq(7, 46, 2)
sampsize <- round(nrow(train_main1) * c(0.7, 0.8))

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

# Create an empty vector to store OOB error values
oob_err <- c()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {
  
  # Train a Random Forest model
  model_rf <- randomForest(formula = Activity ~ ., 
                        data = train_main1,
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i])
  
  # Store OOB error for the model                      
  oob_err[i] <- model_rf$err.rate[nrow(model_rf$err.rate), "OOB"]
}

# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])
#-------------------------------------------------------------



