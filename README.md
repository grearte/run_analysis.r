# **Purpose of run_analysis.r**

#### **A) Merges training and test sets to create one data set**
#### **B) Extracts only the measurements on the mean and standard deviation for each measurement**
#### **C) Uses descriptive activity names to name the activities in the dataset**
#### **D) Appropriately labels the data set with descriptive variable names**
#### **E) From the data set in step D, creates a second, independent tidy data set with the average of each variable for each activity and each subject**

######**1.0 Set directory path, global parameters, load required packages**
######**1.1 Open "features.txt" file; create vector to be used as input for names()**
######**1.2 Open "X" files**
######**1.3 Merge "X" files into a single dataframe**
######**2.1 Open "Subject" files**
######**2.2 Merge "subject" files into a single dataframe**
######**3.1 Open "y" files**
######**3.2 Merge "y" files into a single dataframe**
######**4.1 Open "activity_labels.txt" file**
######**4.2 Create a new column "y_combined$activity" with values from "activity_labels$activity"**

y_combined$activity <-ifelse(y_combined$act_id == 1,"WALKING",
                       ifelse(y_combined$act_id == 2,"WALKING_UPSTAIRS",
                              ifelse(y_combined$act_id ==      3,"WALKING_DOWNSTAIRS",
                                     ifelse(y_combined$act_id == 4,"SITTING",
                                            ifelse(y_combined$act_id == 5,"STANDING","LAYING"
                                            )))))

######**5.0 Merge "X" dataframes**
######**5.1 Merge "X_combined" with "sub_combined" into "combined_5_1"**
######**5.2 Merge "combined_5_1" with "y_combined" into "complete_5_2"**
######**5.3 Sanitizing the names of columns of the data frame as some characters had
######**6.1 Variables with mean() and std() calculations. Selecting columns with either 'mean' or 'mean()' and 'std' or 'std()' is up to indvidual interpretation I assume that more than one person could work developing this data set and there was no naming agreement as to how to properly name a column**
**6.2 Add "subject" and "activity" to each data frame for further individual computation and merge()**
######**7.1 Ordering the data.frame()s by subject, then by activty
**7.2)**

library(plyr) 
inal_mean <- ddply(ordered_mean_df,.(subject, activity),colwise(mean))
final_sd <- ddply(ordered_sd_df,.(subject, activity),colwise(mean))

######**8.1 Merge without sorting both data frames with unique 'subject' and 'activity'**

complete <- merge(final_mean, final_sd, by.x=c("subject", "activity"), by.y=c("subject", "activity"), sort=FALSE)

######**8.2 Output tidy data dataset as per requirements**

write.table(complete, file="tidy_data.txt", row.name=FALSE)

