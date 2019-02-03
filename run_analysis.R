### Set Libraries ###
library(dplyr)
library(data.table)


### Set Working Directory ### 
setwd("~/Coursera/DataScience/03_GettingandCleaningData/UCI HAR Dataset")

################ Read data and prepare column names ###############

## General information ##                  
features <- read.table("~/Coursera/DataScience/03_GettingandCleaningData/UCI HAR Dataset/features.txt")
activity_labels <- read.table("~/Coursera/DataScience/03_GettingandCleaningData/UCI HAR Dataset/activity_labels.txt") %>%
  rename(Activity_name=V2, Activity_Number=V1)

features_label <- features %>% mutate(Name=gsub("^t", "time.", V2)) %>% 
  mutate(Name=gsub("^f", "freq.", Name)) %>%
  mutate(Name=gsub("[()]","", Name,ignore.case=TRUE)) %>%
  mutate(Name=gsub("-", ".", Name)) %>%
  mutate(Col=paste("V", V1,sep="")) %>%
  select(Name,Col) 

relevant_columns <- features_label %>%
                      filter(grepl("mean",Name) | grepl("std",Name))

## Test data ##
X_test <- read.table("~/Coursera/DataScience/03_GettingandCleaningData/UCI HAR Dataset/test/X_test.txt")
X_test_rel <- X_test[ , relevant_columns$Col]
setnames(X_test_rel, old=colnames(X_test_rel), new=c(relevant_columns$Name))

y_test <- read.table("~/Coursera/DataScience/03_GettingandCleaningData/UCI HAR Dataset/test/y_test.txt") %>%
                      rename(Activity=V1)
subject_test <- read.table("~/Coursera/DataScience/03_GettingandCleaningData/UCI HAR Dataset/test/subject_test.txt") %>%
                      rename(Subject_ID=V1)
        

## Train data ##
X_train <- read.table("~/Coursera/DataScience/03_GettingandCleaningData/UCI HAR Dataset/train/X_train.txt")
X_train_rel <- X_train[ , relevant_columns$Col]
setnames(X_train_rel, old=colnames(X_train_rel), new=c(relevant_columns$Name))


y_train <- read.table("~/Coursera/DataScience/03_GettingandCleaningData/UCI HAR Dataset/train/y_train.txt")%>%
                      rename(Activity=V1)
subject_train <- read.table("~/Coursera/DataScience/03_GettingandCleaningData/UCI HAR Dataset/train/subject_train.txt") %>%
                      rename(Subject_ID=V1)
                      




### Merge data tables ###
test_data <- merge(cbind(subject_test, y_test, X_test_rel), activity_labels, by.x="Activity", by.y="Activity_Number") %>%
                      select(-(Activity)) 
train_data <- merge(cbind(subject_train, y_train, X_train_rel), activity_labels, by.x="Activity", by.y="Activity_Number") %>%
                      select(-(Activity)) 

## Combine Test and Train data ##
tidy_data <- rbind(test_data, train_data)
            
## Summary over all Subject-ID/Activity combinations ##
summary <- tidy_data %>% group_by(Subject_ID, Activity_name) %>%
              summarise_all("mean")

