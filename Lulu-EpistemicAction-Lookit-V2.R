# Written by Elif Poyraz on Dec 18, 2024
# Updated on April 7, 2025
# Read csv frame data files downloaded from Lookit.

# need the following packages
library(stringr)

## The first part is for analyzing Version 1- with more frames, showing videos one at a time
# read all responses to get sub IDs
all_resp = read.csv("Lulu-wants-to-learn-about-people-and-objects-_all-responses-identifiableApril7.csv",
                      sep=",", header = TRUE, fill =  TRUE, comment.char = "");

# hard code the file path name for the stim pictures
path_name = "https://raw.githubusercontent.com/elfnrpyrz/Lookit-Lulu/master/img/";

# hard coding the actor IDs - to get the file names
actor_ids = c("AG", "GP", "JM", "RZ", "VR", "GB", "ZZ", "AK", "QZ", "YL", "JE", "FZ");

# hard coding the frame id numbers to keep - these are the only frames where children made a response
frame_ids = c("25-testFrameGroup", "29-testFrameGroup", "33-testFrameGroup", "37-testFrameGroup", "41-testFrameGroup", "45-testFrameGroup", 
              "50-testFrameGroup", "54-testFrameGroup", "58-testFrameGroup", "62-testFrameGroup", "66-testFrameGroup", "70-testFrameGroup");

# create a data frame to append the data
col_names <- c("SubID", "Age", "Trial_no", "Cond", "Q", "Resp", "Accuracy", "F1", "F2");
my_df <- data.frame(matrix(ncol = 9, nrow = 0));
ind_means <- data.frame(matrix(ncol =2, nrow =0));

row_n = 42;  
# iterate over each response excel sheet
for (i in 1:row_n){
  # pre allocate a data frame for each child
  child_df <- data.frame(matrix(data=NA, ncol =9, nrow =12))
  
  # create the file name to read for each subject
  file_name= paste0("Lulu-wants-to-learn-about-people-and-objects-_",
                    all_resp$response__uuid[i],"_frames.csv");
  
  # read individual child response frame
  child_allresp=read.table(file_name, sep = ",", header = TRUE, fill = TRUE); 
  
  #populate the sub ID and age
  child_df[, 1] <- all_resp$child__hashed_id[i];
  child_df[, 2] <- all_resp$child__age_in_days[i]/365*12;
  
  # remove all unnecessary frames, only retain the info with which video shown on which side, and final picture selected
  child = subset(child_allresp,str_detect(child_allresp$frame_id, 'testFrameGroup')==1 & 
                    str_detect(child_allresp$frame_id, 'testIntroFrameGroup')==0 & 
                    str_detect(child_allresp$key, 'feedbackAudio')==0 &
                    str_detect(child_allresp$key, '.src')==1 | 
                    str_detect(child_allresp$key, 'selectedImage')==1 );
  
  for (j in 1:length(actor_ids)){
    child_df[j, 3] = j; # trial number
    per_t= subset(child, str_detect(child$frame_id, frame_ids[j])==1); # get current trial frames
    child_df[j, 4] = gsub(".gif","",gsub(paste0(path_name,actor_ids[j],"_"),"",per_t$value[1])); # save trial Cond - what was shown on left side
    child_df[j, 5] = gsub(".png","",gsub(path_name,"",per_t$value[3])); # save trial Question - asked number or shape?
    child_df[j, 6] = gsub(paste0("trial", j),"",per_t$value[4]); # save child response for current trial
    
    if (is.na(child_df[j, 6])== FALSE){
      #if child chose the left image and the left image and question match, then child is accurate
      if (child_df[j, 6]=='l' & substring(child_df[j, 5], 1, 1) == child_df[j, 4]){child_df[j, 7] = 1}
      else if (child_df[j, 6]=='l' & substring(child_df[j, 5], 1, 1) != child_df[j, 4]){child_df[j, 7] = 0};
      
      #if child chose the right image and the left image and question match, then child is inaccurate
      if (child_df[j, 6]=='r' & substring(child_df[j, 5], 1, 1) == child_df[j, 4]){child_df[j, 7] = 0}
      else if (child_df[j, 6]=='r' & substring(child_df[j, 5], 1, 1) != child_df[j, 4]){child_df[j, 7] = 1};}
    
  }
  if (child$key[1]== 'selectedImage'){
    # save child's response to first feedback question (how much did you like the game?) (higher, more liking)
    if (child$value[1]=="opt3"){child_df[1,8]=3} else if (child$value[1]=="opt2"){child_df[1,8]=2} else {child_df[1,8]=1};
    
    # save child's response to second feedback question (did you find the game easy or hard?) (higher, more difficult)
    if (child$value[2]=="opt3"){child_df[1,9]=3} else if (child$value[2]=="opt2"){child_df[1,9]=2} else {child_df[1,9]=1};
  }
  
  ind_means <- rbind(ind_means, c(round(mean(child_df[, 7]*100, na.rm = TRUE)),all_resp$child__age_in_days[i]/365*12));
  #append child's response to the big data set
  my_df <- rbind(my_df, child_df)
}

colnames(my_df)<- col_names;
colnames(ind_means)<- c("Mean", "Age");

# save the clean data sheet into a CSV
write.csv(my_df, file = "clean_dataV1.csv");

#save the individual means and age into a CSV
write.csv(ind_means, file = "meansV1.csv");
mean(my_df$Accuracy, na.rm =TRUE)
#hist(ind_means[1])

#################
## The second part is for analyzing Version 2- with less frames, NOT showing videos one at a time
# read all responses to get sub IDs
all_resp = read.table("Lulu-wants-to-learn-about-people-and-objects-_all-responses-identifiableApril7.csv",
                      sep=",", header = TRUE, fill =  TRUE);

# hard code the file path name for the stim pictures
path_name = "https://raw.githubusercontent.com/elfnrpyrz/Lookit-Lulu/master/img/";

# hard coding the actor IDs - to get the file names
actor_ids = c("AG", "GP", "JM", "RZ", "VR", "GB", "ZZ", "AK", "QZ", "YL", "JE", "FZ");

# hard coding the frame id numbers to keep - these are the only frames where children made a response
frame_ids = c("23-testFrameGroup", "25-testFrameGroup", "27-testFrameGroup", "29-testFrameGroup", "31-testFrameGroup", "33-testFrameGroup", 
              "36-testFrameGroup", "38-testFrameGroup", "40-testFrameGroup", "42-testFrameGroup", "44-testFrameGroup", "46-testFrameGroup");

# create a data frame to append the data
col_names <- c("SubID", "Age", "Trial_no", "Cond", "Q", "Resp", "Accuracy", "F1", "F2");
my_df <- data.frame(matrix(ncol = 9, nrow = 0));
ind_means <- data.frame(matrix(ncol =2, nrow =0));

row_n = 42; 
# iterate over each response excel sheet
for (i in (row_n+1):length(all_resp$response__uuid)){
  # pre allocate a data frame for each child
  child_df <- data.frame(matrix(data=NA, ncol =9, nrow =12))
  
  # create the file name to read for each subject
  file_name= paste0("Lulu-wants-to-learn-about-people-and-objects-_",
                    all_resp$response__uuid[i],"_frames.csv");
  
  # read individual child response frame
  child_allresp=read.table(file_name, sep = ",", header = TRUE, fill = TRUE); 
  
  #populate the sub ID and age
  child_df[, 1] <- all_resp$child__hashed_id[i];
  child_df[, 2] <- all_resp$child__age_in_days[i]/365*12;
  
  # remove all unnecessary frames, only retain the info with which video shown on which side, and final picture selected
  child = subset(child_allresp,str_detect(child_allresp$frame_id, 'testFrameGroup')==1 & 
                   str_detect(child_allresp$frame_id, 'testIntroFrameGroup')==0 & 
                   str_detect(child_allresp$key, 'feedbackAudio')==0 &
                   str_detect(child_allresp$key, '.src')==1 | 
                   str_detect(child_allresp$key, 'selectedImage')==1 );
  
  for (j in 1:length(actor_ids)){
    child_df[j, 3] = j; # trial number
    per_t= subset(child, str_detect(child$frame_id, frame_ids[j])==1); # get current trial frames
    child_df[j, 4] = gsub(".gif","",gsub(paste0(path_name,actor_ids[j],"_"),"",per_t$value[1])); # save trial Cond - what was shown on left side
    child_df[j, 5] = gsub(".png","",gsub(path_name,"",per_t$value[3])); # save trial Question - asked number or shape?
    child_df[j, 6] = gsub(paste0("trial", j),"",per_t$value[4]); # save child response for current trial
    
    if (is.na(child_df[j, 6])== FALSE){
      #if child chose the left image and the left image and question match, then child is accurate
      if (child_df[j, 6]=='l' & substring(child_df[j, 5], 1, 1) == child_df[j, 4]){child_df[j, 7] = 1}
      else if (child_df[j, 6]=='l' & substring(child_df[j, 5], 1, 1) != child_df[j, 4]){child_df[j, 7] = 0};
      
      #if child chose the right image and the left image and question match, then child is inaccurate
      if (child_df[j, 6]=='r' & substring(child_df[j, 5], 1, 1) == child_df[j, 4]){child_df[j, 7] = 0}
      else if (child_df[j, 6]=='r' & substring(child_df[j, 5], 1, 1) != child_df[j, 4]){child_df[j, 7] = 1};}
    
  }
  if (child$key[1]== 'selectedImage'){
    # save child's response to first feedback question (how much did you like the game?) (higher, more liking)
    if (child$value[1]=="opt3"){child_df[1,8]=3} else if (child$value[1]=="opt2"){child_df[1,8]=2} else {child_df[1,8]=1};
    
    # save child's response to second feedback question (did you find the game easy or hard?) (higher, more difficult)
    if (child$value[2]=="opt3"){child_df[1,9]=3} else if (child$value[2]=="opt2"){child_df[1,9]=2} else {child_df[1,9]=1};
  }
  
  ind_means <- rbind(ind_means, c(round(mean(child_df[, 7]*100, na.rm = TRUE)),all_resp$child__age_in_days[i]/365*12));
  #append child's response to the big data set
  my_df <- rbind(my_df, child_df)
}

colnames(my_df)<- col_names;
colnames(ind_means)<- c("Mean", "Age");

# save the clean data sheet into a CSV
write.csv(my_df, file = "clean_dataV2.csv");

#save the individual means and age into a CSV
write.csv(ind_means, file = "meansV2.csv");
mean(my_df$Accuracy, na.rm =TRUE)
#hist(ind_means[1])
