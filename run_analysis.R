run_analysis <- function()
{
  #   1.Merges the training and the test sets to create one data set.
  #   2.Extracts only the measurements on the mean and standard deviation for each measurement. 
  #   3.Uses descriptive activity names to name the activities in the data set
  #   4.Appropriately labels the data set with descriptive variable names. 
  #   5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
  
  ###### Create the new tidy Data Frame ######
  ## Columns for the new data frame:
  ## Test_Or_Train  : used to distinguishes between "Train" records (from the train directory) from "Test" records (from test directory)
  ## Feature        : this column to contain the feature name as defined in features.txt file
  ## Subject        : this is the id of the person that particiapted in the survey. this data is retrieved from subject_train.txt
  ## Activity       : this is the activity name as defined in the y_train.txt
  ## Measurement    : this is the measure calculation which is the mean of the available values
  
  #create the data frame with the expected number of columns. In Future: I can get this number dynamically. For now, I'm hardcoding it.
  #tidy <- data.frame(Test_Or_Train=NA, Feature=NA, Subject=NA, Activity=NA, Measurement=NA)
  ### Remove the first row with NAs
  #tidy <- tidy[-1,] 
  
 
  
  ###### LOOP to read TEST data and TRAIN data #######
  testtrain = c("test", "train")
  for (folder in testtrain)
  {
    print("####")
    print(folder)
    print("####")
      
    ## construct the file names
    ActivitiesFile <- paste("./UCI HAR Dataset/",folder,"/y_", folder, ".txt", sep="")
    #print(ActivitiesFile)
    ActivitiesReferenceFile <- "./UCI HAR Dataset/activity_labels.txt"
    #print(ActivitiesReferenceFile)
    SubjectsFile <- paste("./UCI HAR Dataset/",folder,"/subject_", folder, ".txt", sep="")
    #print(SubjectsFile)
    MeasurementsFile <- paste("./UCI HAR Dataset/",folder,"/X_", folder, ".txt", sep="")
    #print(MeasurementsFile)
    FeaturesReferenceFile <- "./UCI HAR Dataset/features.txt"
    #print(MEasurementsReferenceFile)
    
    ######## ACTIVITIES ##########################
    ## read the activity file (y_train.txt)
    Activities <- read.csv(ActivitiesFile, header=FALSE, col.names="idToname", stringsAsFactors=FALSE)
    #print(head(Activities))
    numRows <- nrow(Activities)
    #print(r)
    
    ## read the activities reference file
    ActivitiesRef <- read.csv(ActivitiesReferenceFile, header = FALSE, sep=" ", col.names=c("id", "Activity"), stringsAsFactors=FALSE)
    #print(ActivitiesRef)
    #print("###1##")
    
    ## replace the ids in Activities with names
    #mergeActivities <- merge(Activities, ActivitiesRef, by.x="id", by.y="id", all.x=TRUE, all.y=FALSE)
    #print(tail(mergeActivities, n=20))
    #print("###2##")
    #write.csv(mergeActivities, "./MergedActivities.csv")
    #ActivityNames = replace(Activities[[1]], ActivitiesRef[["id"]], ActivitiesRef[["name"]])
    
    ## convert to the name of the activity insead of id.   
    for(i in 1:numRows) 
    {
        #print(paste(i,"\r"))
        #replacing with the name
        id = Activities[i,"idToname"]
        Activities[i,"idToname"] <- ActivitiesRef[id,"Activity"]
        #print(paste(id, "--", Activities[i,"idToname"]))
        #tidy[i,"Activity"] <- ActivitiesRef[Activities[i,"id"],"id"]
        #       print("$$$$$")
        #       print(Activities[i,"id"])
        #       # filter the Activities reference to exactly one row
        #       ref <- ActivitiesRef["id"==as.integer(Activities[i,"id"]),]
        #       print(ref)
        #       # check for error (reference not found)
        #       if (nrow(ref)==0) return("ERROR - An Activity reference not found in Activity_labels.txt")
        #       # add the row to tidy
        #       tidy[i,"Activity"] <- ref[1,"Activity"]
      
    }
    #print(paste("i=",i))
    #write.csv(Activities, "./MergedActivities.csv")
    ## add the activity data into tidy
    #tidy$Activity <- mergeActivities$name
    #row.names(tidy) <- seq(1,nrow(mergeActivities),1)
    # add the expected number of rows to tidy (keep the same number of columns)
    #dt <- dim(tidy)
    #print(dt)
    #print(dt[1])
    #print(dt[2])
    #print(nrow(mergeActivities))
    #dim(tidy) <- c(2948,5)
    #dim(tidy)[1] <- dt[1]+nrow(mergeActivities)
    #print(dim(tidy))
    #cbind(tidy, mergeActivities$Activity)
    # tidy <- cbind(tidy,mergeActivities$Activity)
    # note: The cbind data frame method is just a wrapper for data.frame(..., check.names = FALSE). 
    #       This means that it will split matrix columns in data frame arguments, 
    #       and convert character columns to factors unless stringsAsFactors = FALSE is specified.
    
    ####### SUBJECTS ############################
    ## read the subject (subject_train.txt)
    Subjects <- read.csv(SubjectsFile, header=FALSE, col.names="subjects", stringsAsFactors=FALSE)
    
    ## check if all dimensions are equal
    #if (nrow(Activities)!=nrow(Subjects)) return ("ERROR--number of Activities rows DO NOT MATCH number of Subjects rows")
        
    ## add the subjects data into tidy
    
    
    ####### MEASUREMENTS ########################
    ## read the measurements and MeasurementsRef Files
    #Measurements <- read.csv(MeasurementsFile, header=FALSE, stringsAsFactors=FALSE, sep=" ", , strip.white=TRUE)
    
    ######################
    ## read the Features file and Create 2 vectors by searching for mean and std: 
    ## the first vector to be used by read.fwf a
    ## the second vector to be used to set the column names of the tidy data frame  
    FeaturesRef <- read.csv(FeaturesReferenceFile, header = FALSE, sep=" ", col.names=c("id", "Feature"), stringsAsFactors=FALSE)
    #### read the csv one row at a time (for loop). 
    ### if the text has mean or standard deviation then:
    ###       add rep(-1,16) to the fwf vector, else add -16 to the fwf vector
    ###       add the name to the columnnames vector
    #g <- grep("std|[Mm]ean", FeaturesRef$Feature, value=TRUE)
    #fwf <- data.frame()
    #tidyFeaturesColNames <- data.frame()
    fwf <- NULL
    tidyFeaturesColNames <- NULL
    for (i in 1:nrow(FeaturesRef))
    {
      g <- grep("std|[Mm]ean", FeaturesRef[i,"Feature"], value=TRUE)
      if (length(g)>0)
      {
        ## first vector
        fwf <- c(fwf,-1,15)
        
        ## second vector
        tidyFeaturesColNames <- c(tidyFeaturesColNames, FeaturesRef[i,"Feature"])
        #print(paste("FOUND-",i,FeaturesRef[i,"Feature"]))
      }
      else
      {
        ## first vector
        fwf <- c(fwf,-16)
        #print(paste("NOT FOUND-",i,FeaturesRef[i,"Feature"]))
      }
      
    }
    
    #################################
      
     
    
    #w <- rep(c(-1, 15), times=nrow(FeaturesRef))
#     fwf <- c(rep(c(-1,15),6),(-16*34),rep(c(-1,15),6),(-16*34),rep(c(-1,15),6),(-16*34),rep(c(-1,15),6),(-16*34),rep(c(-1,15),6),(-16*34),
#            rep(c(-1,15),2),(-16*11),rep(c(-1,15),2),(-16*11),rep(c(-1,15),2),(-16*11),rep(c(-1,15),2),(-16*11),rep(c(-1,15),2),(-16*11),
#            rep(c(-1,15),6),(-16*73),rep(c(-1,15),6),(-16*73),rep(c(-1,15),6),(-16*73),
#            rep(c(-1,15),2),(-16*11),rep(c(-1,15),2),(-16*11),rep(c(-1,15),2),(-16*11),rep(c(-1,15),2),(-16*11),
#            rep(c(-1,15),7))
    #print(w)
    #print(nrow(FeaturesRef))
    Measurements <- read.fwf(MeasurementsFile, header=FALSE, widths=fwf)

    
    ## check if all dimensions are equal
    #print(paste("num rows activities=",numRows))
    #print(paste("num rows measurements=", nrow(Measurements)))
    #print(head(Measurements))
    #print(tail(Measurements))
    if (nrow(Measurements) != numRows) return("ERROR -- Number of rows of Measurements File and Activities File DO NOT MATCH")
    print(paste("num col Measurements=",ncol(Measurements)))
    #print(paste("num rows features=", nrow(FeaturesRef)))
    #if (ncol(Measurements) != nrow(FeaturesRef)) return("ERROR - num rows of Features.txt DO NOT MATCH number of columns of X_*.txt file")
    
    ## pick the means and standard deviation columns and 
    
    
    
        
    
    ###### ADD all created columns to data frmame tidy
    ##tidy = as.data.drame(cbind(mergeActivities, Subjects, Measure1, Measure2 ))
    if (folder=="test")
    {
      TestDF <- data.frame(TestOrTrain=rep("Test",numRows))
      testtidy <- cbind(TestDF, Activities, Subjects, Measurements)
    }
    else
    {
      TrainDF <- data.frame(TestOrTrain=rep("Train",numRows))
      traintidy <- cbind(TrainDF, Activities, Subjects, Measurements)
    }
    
  }
    
  tidy <- rbind(testtidy,traintidy)
  colnames(tidy) <- c("Test_Or_Train", 
                      "Activity", 
                      "Subject",
                      tidyFeaturesColNames)
#                       "tBodyAcc-mean()-X",
#                       "tBodyAcc-mean()-Y",
#                       "tBodyAcc-mean()-Z",
#                       "tBodyAcc-std()-X",
#                       "tBodyAcc-std()-Y",
#                       "tBodyAcc-std()-Z",
#                       "GravityAcc-mean()-X",
#                       "tGravityAcc-mean()-Y",
#                       "tGravityAcc-mean()-Z",
#                       "tGravityAcc-std()-X",
#                       "tGravityAcc-std()-Y",
#                       "tGravityAcc-std()-Z",
#                       "tBodyAccJerk-mean()-X",
#                       "tBodyAccJerk-mean()-Y",
#                       "tBodyAccJerk-mean()-Z",
#                       "tBodyAccJerk-std()-X",
#                       "tBodyAccJerk-std()-Y",
#                       "tBodyAccJerk-std()-Z",
#                       "tBodyGyro-mean()-X",
#                       "tBodyGyro-mean()-Y",
#                       "tBodyGyro-mean()-Z",
#                       "tBodyGyro-std()-X",
#                       "tBodyGyro-std()-Y",
#                       "tBodyGyro-std()-Z",
#                       "tBodyGyroJerk-mean()-X",
#                       "tBodyGyroJerk-mean()-Y",
#                       "tBodyGyroJerk-mean()-Z",
#                       "tBodyGyroJerk-std()-X",
#                       "tBodyGyroJerk-std()-Y",
#                       "tBodyGyroJerk-std()-Z",
#                       "tBodyAccMag-mean()",
#                       "tBodyAccMag-std()",
#                       "tGravityAccMag-mean()",
#                       "tGravityAccMag-std()",
#                       "tBodyAccJerkMag-mean()",
#                       "tBodyAccJerkMag-std()",
#                       "tBodyGyroMag-mean()",
#                       "tBodyGyroMag-std()",
#                       "tBodyGyroJerkMag-mean()",
#                       "tBodyGyroJerkMag-std()",
#                       "fBodyAcc-mean()-X",
#                       "fBodyAcc-mean()-Y",
#                       "fBodyAcc-mean()-Z",
#                       "fBodyAcc-std()-X",
#                       "fBodyAcc-std()-Y",
#                       "fBodyAcc-std()-Z",
#                       "fBodyAccJerk-mean()-X",
#                       "fBodyAccJerk-mean()-Y",
#                       "fBodyAccJerk-mean()-Z",
#                       "fBodyAccJerk-std()-X",
#                       "fBodyAccJerk-std()-Y",
#                       "fBodyAccJerk-std()-Z",
#                       "fBodyGyro-mean()-X",
#                       "fBodyGyro-mean()-Y",
#                       "fBodyGyro-mean()-Z",
#                       "fBodyGyro-std()-X",
#                       "fBodyGyro-std()-Y",
#                       "fBodyGyro-std()-Z",
#                       "fBodyAccMag-mean()",
#                       "fBodyAccMag-std()",
#                       "fBodyBodyAccJerkMag-mean()",
#                       "fBodyBodyAccJerkMag-std()",
#                       "fBodyBodyGyroMag-mean()",
#                       "fBodyBodyGyroMag-std()",
#                       "fBodyBodyGyroJerkMag-mean()",
#                       "fBodyBodyGyroJerkMag-std()",
#                       "angle(tBodyAccMean,gravity)",
#                       "angle(tBodyAccJerkMean),gravityMean)",
#                       "angle(tBodyGyroMean,gravityMean)",
#                       "angle(tBodyGyroJerkMean,gravityMean)",
#                       "angle(X,gravityMean)",
#                       "angle(Y,gravityMean)",
#                       "angle(Z,gravityMean)")

  
  write.csv(tidy, "./TIDY.csv")

  ######### Second TIDY ###########
#   ## refactor / split the data
#   ## save 2nd tidy 
#   #write.csv(tidy2, "./TIDY2.csv")
#   tidy2 = split(tidy, tidy$Activity)
#   for (i=1 in 1:length(tidy2))
#   {
#     tidy2[[i]] <- split(tidy2[[i]], , tidy$Subject )
#   }
#   write.csv(tidy2, "./TIDY2.csv")
  
######### Second TIDY ###########
## refactor / split the data
## save 2nd tidy 
#write.csv(tidy2, "./TIDY2.csv")
#print("@@@@@")
#tidy2 = split(tidy, factor(tidy$Activity))
#print(head(tidy2))
#print("####")
#for (i in 1:length(tidy2))
#{
#  tidy2[[i]] <- split(tidy2[[i]], factor(tidy$Subject ))
#}


### option 2
#tidy2 <- split(tidy, factor(tidy$Activity)) 
#print("####333")
#sapply(tidy2, function(x) colMeans(x[,c(seq(4,73,1))])) 
#3:73

  ### option 3
  tidy2 <- split(tidy, list(factor(tidy$Activity), factor(tidy$Subject) )) #, factor(tidy$Test_Or_Train)))
  #print("####444")
  tidy2 <- sapply(tidy2, function(x) colMeans(x[,c(seq(4,ncol(tidy),1))])) 
  #print(head(tidy2))
  
  write.csv(tidy2, "./TIDY2.csv")
}