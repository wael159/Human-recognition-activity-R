#Naive bayes
install.packages("naivebayes")
library(naivebayes)
install.packages("dplyr")
library("dplyr")
# Build the Activity  prediction model
locmodel <- naive_bayes(Activity~., data = train_main1)


# Predict tesT aCTIVITY
activity_predicted=predict(locmodel, newdata=test)
cm_naive=confusionMatrix(activity_predicted, xtest_target)
plot_con_matrixc(cm_naive)
table_by_class=cm_naive$byClass   #cm by class 
summary_confustion(table_by_class)

#take F1,precision,recall,accuarcy
as.data.frame(table_by_class)%>%select(Precision,Recall,F1,'Balanced Accuracy')

#summary of accuracy/F1/PRECISION 
summary_confustion=function(x){
table_by_class=as.data.frame(x)
tot_con=table_by_class %>% 
  summarise(Specificity=mean(Specificity),Sensitivity=mean(Sensitivity),recall=mean(Recall),F1=mean(F1),Acuracy=mean(`Balanced Accuracy`))
total=list(table_by_class,tot_con)
return(total)
}


#model with cleaned data with 86 features

Actmodel= naive_bayes(Activity~., data = train_cleaned)
activity_predicted1=predict(Actmodel, newdata=test)
cm_naive1=confusionMatrix(activity_predicted1, xtest_target)
plot_con_matrixc(cm_naive1)