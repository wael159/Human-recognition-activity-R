#-------from github --cleaning the data
#merge the train and test
raw_data=bind_rows(train_main1,test)

## Extract only the measurements on the mean and standard deviation for each measurement
sub_act <- raw_data %>%
  select(subject, Activity)
features <- raw_data %>%
  select(-subject, -Activity)

### getting the feature names provided with the data set
colnames(features)

sub <- grepl(pattern = "(mean|std)", x = colnames(features))
features1 <-
  features[, sub]
colnames(features1)
# merging label and features datasets
raw_data1 <-
  bind_cols(sub_act, features1) 

## Appropriately label the data set with descriptive variable names
raw_data1 <- raw_data1 %>%
  gather(data = ., variable, value, -subject, -Activity) %>%
  separate(
    col = variable,
    into = c("signal", "parameter", "axis"),
    convert = TRUE
  )

raw_data1 <- raw_data1 %>%
  extract(
    col = signal,
    into = "domain",
    regex = "^(t|f)",
    remove = FALSE
  )%>%
  
  extract(
    col = signal,
    into = "instrument",
    regex = ("(Acc|Gyro)"),
    remove = FALSE
  )%>%
  
  extract(
    col = signal,
    into = "acceleration",
    regex = ("(Body|Gravity)"),
    remove = FALSE
  )  %>%
  extract(col = signal, into = "signal", regex = "(JerkMag|Jerk|Mag)")

raw_data1$domain <-
  gsub(pattern = "t",
       replacement = "time",
       x = raw_data1$domain)

raw_data1$domain <-
  gsub(pattern = "f",
       replacement = "frequency",
       x = raw_data1$domain)

raw_data1$instrument <-
  gsub(pattern = "Acc",
       replacement = "accelerometer",
       x = raw_data1$instrument)

raw_data1$instrument <-
  gsub(pattern = "Gyro",
       replacement = "gyroscope",
       x = raw_data1$instrument)

raw_data1$Activity=as.factor(raw_data1$Activity)
levels(raw_data1$Activity)

### final details
tidy_data <- raw_data1 %>%
  mutate(
    axis = replace(x = axis, which(axis == ""), NA),
    parameter = replace(x = parameter, which(parameter == "meanFreq"), "mean")
  )
tidy_data <- tidy_data %>%
  mutate_if(is.character, tolower)

write_csv(x = tidy_data, path = "~tidy_data.csv")
