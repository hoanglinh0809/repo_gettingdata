get_data <- function(directory, subject_filename,activity_filename,data_filename){
  
  ##########################################
  ##########################################
  # read file subject_train.txt
  # An identifier of the subject who carried out the experiment.
  filepath <- paste(directory,subject_filename, sep = "")
  idCol <- read.csv(filepath,header=FALSE)
  colnames(idCol) <- c("Subject ID")
  SubjectID <- as.numeric(idCol[[1]])
  ##########################################
  ##########################################
  
  ##########################################
  ##########################################
  # read file y_train.txt
  # Activity label
  filepath <- paste(directory,activity_filename, sep = "")
  activityCol <- read.csv(filepath,header=FALSE)
  colnames(activityCol) <- c("Activity")
  Activity <- as.character(activityCol[[1]])
  ActivityName <- c()
  #1-WALKING, 2-WALKING_UPSTAIRS, 3-WALKING_DOWNSTAIRS, 4-SITTING, 5-STANDING, 6-LAYING
  for(i in 1:length(Activity)){
    if(Activity[i] == 1)
      ActivityName <- c(ActivityName,'WALKING')
    else if(Activity[i] == 2)
      ActivityName <- c(ActivityName,'WALKING_UPSTAIRS')
    else if(Activity[i] == 3)
      ActivityName <- c(ActivityName,'WALKING_DOWNSTAIRS')
    else if(Activity[i] == 4)
      ActivityName <- c(ActivityName,'SITTING')
    else if(Activity[i] == 5)
      ActivityName <- c(ActivityName,'STANDING')
    else if(Activity[i] == 6)
      ActivityName <- c(ActivityName,'LAYING')
  }
  ##########################################
  ##########################################
  
  ##########################################
  ##########################################
  # read file X_train.txt
  # data values for total acceleration, estimated body acceleration, velocity, etc
  
  # first need to find index of values for mean and std
  # make sure file features.txt in the current working directory
  value_names <- read.csv("features.txt",sep = "", header = FALSE)
  value_names_index <- c(grep("std",value_names$V2),grep("mean",value_names$V2))
  value_names_index <- sort(value_names_index)
  
  # create header for these data values
  header <- as.character(value_names$V2[value_names_index])
  
  # read data
  filepath <- paste(directory,data_filename, sep = "")
  value_data <- read.csv(filepath,header=FALSE,stringsAsFactors=FALSE)
  value_frame <- c() #start with empty table
  for(i in 1:nrow(value_data)){
    str <- value_data$V1[i]
    vec <- as.character(strsplit(str," ")[[1]])
    vec2 <- vec[vec!=""]
    vec3 <- as.numeric(vec2)
    vec4 <- vec3[value_names_index]
    
    value_frame <- rbind(value_frame,vec4)
  }
  ##########################################
  ##########################################
  
  value_frame_rows <- nrow(value_data)
  value_frame_cols <- length(header) # initialize
  data <- matrix(value_frame,ncol=value_frame_cols,dimnames=list(c(),header))
  
  data <- cbind(SubjectID,Activity,ActivityName,data)
  data
}

run_analysis <- function(){
  train_data <- get_data('train','/subject_train.txt','/y_train.txt','/X_train.txt')
  test_data <- get_data('test','/subject_test.txt','/y_test.txt','/X_test.txt')
  data <- rbind(train_data, test_data) 
  write.csv(data,"tidy_data.csv",quote = FALSE,row.names = FALSE)
  
  #filter data & calculate mean of each variables
  filter_subjectID <- data[1:nrow(data),1]
  subjectID_range <- unique(filter_subjectID)
  filter_activity <- data[1:nrow(data),2]
  activity_range <- unique(filter_activity)
  
  tidy_data <- c()
  for(subjectID in subjectID_range){
    for(activity in activity_range){
      filter <- (filter_subjectID==subjectID & filter_activity==activity)
      filter_data <- data[filter,1:ncol(data)]
      
      vec <- c(filter_data[1,1],filter_data[1,2],filter_data[1,3])
      for(i in 4:ncol(filter_data)){
        vec <- c(vec, mean(as.numeric(filter_data[1:nrow(filter_data),i])))
      }
      
      tidy_data <- rbind(tidy_data,vec)
    }
  }
  
  tidy_data_header <- colnames(filter_data)
  tidy_data_cols <- length(tidy_data_header) # initialize
  data2 <- matrix(tidy_data,ncol=tidy_data_cols,dimnames=list(c(),tidy_data_header))
  
  write.csv(data2,"tidy_data2.csv",quote = FALSE,row.names = FALSE)
  
  data2
}