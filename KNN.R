
#KNN 
library("class")
library(ggplot2)     # to plot
library(gridExtra)   # to put more
library(grid) # plot together
library("dplyr")
library("caret")
library("ggplot2")
library(class)

#------------------------normalization function before knn
normfunc<-function(x){
  return((x-min(x))/(max(x) - min(x)))
}

#train and test after normalization before KNN 
train1<-as.data.frame(lapply(train_main1[,1:561], normfunc))
train1<-as.data.frame(lapply(train1[,1:561], as.numeric))

test1<-as.data.frame(lapply(test[,1:561], normfunc))
test1<-as.data.frame(lapply(test1[,1:561], as.numeric))

#the targets of test and train 
xtest_target<-test_main_labels
xtrain_target<- train_main_labels

train2=train1
train2$Activity=xtrain_target

#--------------KNN----------with k=6
m1<-knn(train = train1, test = test1, cl = train_main_labels, k=17)

#test accuracy & confusion matrix
table(m1 ,xtest_target)
100 * sum(xtest_target== m1)/length(xtest_target)
cm=confusionMatrix(m1, xtest_target)

#take F1,precision,recall,accuarcy
as.data.frame(cm$byClass)%>%select(Precision,Recall,F1,'Balanced Accuracy')


#confusion matrix for the first knn model with k=6-----must change it to k=17
cm_d <- as.data.frame(cm$table)
cm_st <-data.frame(cm$overall[1:6])
colnames(cm_st)="cm overall"
# round the values

# here we also have the rounded percentage values
cm_p <- as.data.frame(prop.table(cm$table))
cm_d$Perc <- round(cm_p$Freq*100,2)
cm_d$Prediction=as.factor(cm_d$Prediction)
levels(cm_d$Prediction)=c("LAYING","SITTING","STANDING","WALKING","DOWN-STAIRS","UP-STAIRS")

# plotting the matrix
cm_d_p <-  ggplot(data = cm_d, aes(x = Prediction , y =  Reference, fill = Freq))+
  geom_tile() +
  geom_text(aes(label = paste("",Freq)), color = 'white', size = 5) +
  theme_dark() +
  guides(fill=FALSE) 

# plotting the matrix with stats
cm_st_p <-  tableGrob(cm_st)
grid.arrange(cm_d_p, cm_st_p,nrow = 1, ncol = 2, 
             top=textGrob("Confusion Matrix and Statistics",gp=gpar(fontsize=25,font=1)))

#------------------cross validation before knn to choose the K 

trControl <- trainControl(method  = "cv",
                          number  = 5)
#knn with cv from k-1 to 10 
fit <- train(Activity ~ .,
             data       = train2,
             method     = "knn",
             trControl  = trControl,
             metric     = "Accuracy",
             )
k_plot=as.data.frame(fit$results[,1:2])

#plot the K with the acurracy
k_bestplot=ggplot(k_plot,aes(x=k,y=Accuracy))+geom_line()+scale_x_discrete(limits=c(seq(1,10,1)))
print(fit); confusionMatrix(fit)     
knn_preds <- predict(fit, newdata = test1)
confusionMatrix(knn_preds,xtest_target)     


#----------------------------------------calculating the best K-------------------------------------

# knn_train, knn_test, train_labels and test_labels are pre-loaded

# Set random seed. Don't remove this line.
set.seed(1)

# Load the class package, define range and accs

range <- 1:round(0.2 * nrow(train))
range1=seq(1:100)
accs <- rep(0, length(range1))
k=1
for (k in range1) {
  
  # Fill in the ___, make predictions using knn: pred
  pred <- knn(train=train1, test=test1, cl=xtrain_target, k = k)
  
  # Fill in the ___, construct the confusion matrix: conf
  conf <- table(test_main_labels, pred)
  
  # Fill in the ___, calculate the accuracy and store it in accs[k]
  accs[k] <- sum(diag(conf))/sum(conf)
}

# Plot the accuracies. Title of x-axis is "k".

acc1=data.frame(k_range=c(range1),accuracy=c(accs))
ggplot(acc1,aes(x=k_range,y=accuracy))+ geom_point(colour="blue") +
  geom_point(data=acc1[which.max(accs),], aes(x=k_range, y=accuracy), colour="red", size=5)+scale_x_continuous(name="K-neighbors",c((1:30)))+geom_smooth()
# Calculate the best k
accs[which.max(accs)]





