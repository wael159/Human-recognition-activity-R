library(caret)
library(gridExtra)   # to put more
library(grid)
#feature selection by tree ------working------------------ 
set.seed(100)
rPartMod <- train(Activity ~ ., data=train_main1, method="rpart")
rpartImp <- varImp(rPartMod)
print(rpartImp)
plot(rpartImp, top = 20, main='Variable Importance')

#-------lasso-----nor working-----
#by lasso
#lasso 
x <- as.matrix(train_main1[,-563])
fit1 = glmnet(x, train_main_labels, family = "multinomial", alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc')

#plot
plot(fit, xvar = "lambda", label = TRUE, type.coef = "2norm")
plot(fit, xvar = "dev", label = TRUE)

plot(fit1)

# ----------------------------------------------LDA--from datacamp------------
library(MASS)

# linear discriminant analysis after coosing the non correleated features-see in the corrlation script-every file(test/train) have 86 features
lda.fit <- lda(Activity~., data = train_cleaned)

# the function for lda biplot arrows
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "yellow", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}


# target classes as numeric
classes <- as.numeric(train_cleaned$Activity)

# plot the lda results
plot(lda.fit, dimen = 2,col=classes)
lda.arrows(lda.fit, myscale = 1)

# Transform training and test data: train_lda, test_lda
train_lda=as.data.frame(predict(lda.fit, train_cleaned))
test_lda <- as.data.frame(predict(lda.fit, test_cleand))


# Fit lm model using 5 x 5-fold CV: model
trainCtrl = trainControl(
  method = "cv", 
  number = 5,
  verboseIter = TRUE
)

# Train model on LDA-preprocessed data: mdl_lda
mdl_lda <- train(class ~ ., data = train_lda,
                 method = "svmLinear", trControl = trainCtrl)

# Predict on LDA-ed test data: lda_preds
lda_preds <- predict(mdl_lda, test_lda)

#confusion matrix
(cm_lda <- confusionMatrix(lda_preds, test_lda$class))

#over all results of cm after lda
cm_lda

#confusion matrix visualization - for lda
cm_lda1 <- as.data.frame(cm_lda$table)
cm_st_lda <-data.frame(cm_lda$overall[1:6]) # overall cm statstic for the LDA
colnames(cm_st_lda)="cm overall"
# round the values

# here we also have the rounded percentage values
cm_d$Perc <- round(cm_p_lda$Freq*100,2)
cm_lda1$Prediction=as.factor(cm_lda1$Prediction)
levels(cm_lda1$Prediction)=c("LAYING","SITTING","STANDING","WALKING","DOWN-STAIRS","UP-STAIRS")
# plotting the matrix
cm_tot_lda <-  ggplot(data = cm_lda1, aes(x = Prediction , y =  Reference, fill = Freq))+
  geom_tile() +
  geom_text(aes(label = paste("",Freq)), color = 'white', size = 5) +
  theme_dark() +
  guides(fill=FALSE) 

# plotting the stats
cm_st_lda <-  tableGrob(cm_st_lda)
grid.arrange(cm_tot_lda, cm_st_lda,nrow = 1, ncol = 2, 
             top=textGrob("Confusion Matrix and Statistics with LDA",gp=gpar(fontsize=25,font=1)))
