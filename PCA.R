#PCA


install.packages("ggfortify")
library(ggfortify)
# standardize the variables
#activity_std <- scale(as.numeric(subset(raw_data1,select=-Activity)))
activity_std<-as.data.frame(lapply(raw_data1[,1:561], scale))
activity_std[c("Activity")]=raw_data1$Activity
activity_std$Activity
# print out summaries of the standardized variables
summary(activity_std)
# perform principal component analysis (with the SVD method)
pca_activity <- prcomp(activity_std)
autoplot(pca_activity,data=activity_std, colour = 'Activity')


