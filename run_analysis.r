#############################################################################################
##  run_analysis.r -- Last update 12/15/2014
##
##  Written by: GUSTAVO REARTE, Coursera ID 1343156
##
##  Purpose:
##
## A) Merges the training and the test sets to create one data set.
## B) Extracts only the measurements on the mean and standard deviation for each measurement. 
## C) Uses descriptive activity names to name the activities in the data set
## D) Appropriately labels the data set with descriptive variable names. 
## E) From the data set in step D, creates a second, independent tidy data set with the 
##    average of each variable for each activity and each subject.
##
#############################################################################################
#
# (0.1) - Set directory path, global parameters, load required packages
#
set.seed(1)
install.packages("plyr") # library(plyr) called in section 7.2
#
# (1.1) - Open "features.txt" file; create vector to be used as input for names()
#
X_features <- read.csv("~/UCI HAR Dataset/features.txt", sep="", stringsAsFactors=FALSE, header=FALSE)
X_features <- X_features[,2]
#
# (1.2) - Open "X" files; merge into a single data.frame()
#
X_train <- read.csv("~/UCI HAR Dataset/train/X_train.txt", sep="", header=TRUE)
names(X_train) <- X_features
#
X_test  <- read.csv("~/UCI HAR Dataset/test/X_test.txt", sep="", header=TRUE)
names(X_test) <- X_features
#
# (1.3) - Merge "X" files
#
X_combined <- rbind(X_train, X_test) 
#
# (2.1) - Open "Subject" files; merge into a single data.frame()
#
subj_train <- read.csv("~/UCI HAR Dataset/train/subject_train.txt", sep="", header=TRUE)
names(subj_train) <- "subject"
#
subj_test <- read.csv("~/UCI HAR Dataset/test/subject_test.txt", sep="", header=TRUE)
names(subj_test) <- "subject"
#
# (2.2) - Merge "subject" files
#
subj_combined <- rbind(subj_train, subj_test) 
#
# (3.1) - Open "y" files
#
y_train <- read.csv("~/UCI HAR Dataset/train/y_train.txt", sep="", header=TRUE)
names(y_train) <- "act_id"
#
y_test  <- read.csv("~/UCI HAR Dataset/test/y_test.txt", sep="", header=TRUE)
names(y_test) <- "act_id"
#
# (3.2) - Merge "y" files
#
y_combined <- rbind(y_train, y_test) 
#
# (4.1) - Open "activity_labels.txt" file
#
activity_labels <- read.csv("~/UCI HAR Dataset/activity_labels.txt", sep="", stringsAsFactors=FALSE, header=FALSE)
names(activity_labels) <- c("id", "activity")
#
# (4.2) - Create a new column "y_combined$activity" with values from "activity_labels$activity"
#         conditioned by "y_combined$act_id == activity_labels$id"
#
y_combined$activity <-ifelse(y_combined$act_id == 1,"WALKING",
                       ifelse(y_combined$act_id == 2,"WALKING_UPSTAIRS",
                              ifelse(y_combined$act_id == 3,"WALKING_DOWNSTAIRS",
                                     ifelse(y_combined$act_id == 4,"SITTING",
                                            ifelse(y_combined$act_id == 5,"STANDING","LAYING"
                                            )))))
#
# (5.0) - Merge all data.frames()
# 
#       (5.1) - Merge "X_combined" with "sub_combined" into "combined_5_1"
#
combined_5_1 <- cbind(X_combined, subj_combined)
#
#       (5.2) - Merge "combined_5_1" with "y_combined" into "complete_5_2"
#
complete_5_2 <- cbind(combined_5_1, y_combined)
#
#       (5.3) - Sanitizing the names of columns of the data frame as some characters had
#               a negative reaction to 'dplyr' functions (and I presume 'plyr' potentially)
#
col_names <- tolower(names(complete_5_2))
col_names <- gsub("-","_", col_names)
col_names <- sub("\\()","_", col_names)
col_names <- sub("\\(","", col_names)
col_names <- sub("\\)","", col_names)
col_names <- sub("__","_", col_names)
col_names <- sub(",","_", col_names)
col_names <- sub("_$","", col_names)
col_names <- sub(")$","", col_names)
#
colnames(complete_5_2) <- col_names
#
# (6.1) - variables with mean() and std() calculations
#
# Per David Hood's Project FAQ @ https://class.coursera.org/getdata-016/forum/thread?thread_id=50
# selecting columns with either 'mean' or 'mean()' and 'std' or 'std()' is up to indvidual interpretation
# I assume that more than one person could work developing this data set and
# there was no naming agreement as to how to properly name a column (variable / feature)
#
col_names <- unique(tolower(names(complete_5_2))) 
#
mean_v <- grep("mean",col_names, value=T)
#
sd_v <- grep("std",col_names,value=T)
#
only_mean_df <- complete_5_2[, colnames(complete_5_2) %in% mean_v] 
#
only_sd_df <- complete_5_2[, colnames(complete_5_2) %in% sd_v] 
#
#  (6.2) - add "subject" and "activity" to each data frame for further individual computation and merge()
#
mean_df <- cbind(complete_5_2[,c(562,564)], only_mean_df)
#
sd_df <- cbind(complete_5_2[,c(562,564)], only_sd_df)
#
# (7.1) - ordering the data.frame()s by subject, then by activty
#
ordered_mean_df <- mean_df[order(mean_df$subject, mean_df$activity), ]
#
ordered_sd_df <- sd_df[order(sd_df$subject, sd_df$activity), ]
#
# (7.2)
#
library(plyr) 
final_mean <- ddply(ordered_mean_df,.(subject, activity),colwise(mean))
#
final_sd <- ddply(ordered_sd_df,.(subject, activity),colwise(mean))
#
# (8.1) - merge without sorting both data frames with unique 'subject' and 'activity' 
#           
complete <- merge(final_mean, final_sd, by.x=c("subject", "activity"), by.y=c("subject", "activity"), sort=FALSE)
#
# 8.2) - output tidy data dataset as per requirements
#
write.table(complete, file="tidy_data.txt", row.name=FALSE)
########################## < END OF run_analysis.r > ###################################
