run_analysis <- function()
{
  ######## Assignment ################
  #   1.Merges the training and the test sets to create one data set.
  #   2.Extracts only the measurements on the mean and standard deviation for each measurement. 
  #   3.Uses descriptive activity names to name the activities in the data set
  #   4.Appropriately labels the data set with descriptive variable names. 
  #   5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
  ####################################
  
  #########
  ## This code will create 2 data sets
  ##   - tidy.csv : 1st dataset that integrates Test data with Train data. see tidy_code_book.txt for more details on the columns
  ##   - tidy2.txt: 2nd dataset that average of each variable for each activity and each subject. see tidy2_code_book.txt for more details on the columns
  ##
  ## see CodeBook.md for more details on the columns
  #########
  
  
  ###### LOOP to read TEST data and then TRAIN data #######
  testtrain = c("test", "train")
  for (folder in testtrain)
  {
    print(paste("Processing ", folder, " data......."))
      
    ## construct the file names
    ActivitiesFile <- paste("./UCI HAR Dataset/",folder,"/y_", folder, ".txt", sep="")
    ActivitiesReferenceFile <- "./UCI HAR Dataset/activity_labels.txt"
    SubjectsFile <- paste("./UCI HAR Dataset/",folder,"/subject_", folder, ".txt", sep="")
    MeasurementsFile <- paste("./UCI HAR Dataset/",folder,"/X_", folder, ".txt", sep="")
    FeaturesReferenceFile <- "./UCI HAR Dataset/features.txt"
    
    ######## ACTIVITIES data ##########################
    ## read the activity file (y_train.txt) and capture the number of rows
    Activities <- read.csv(ActivitiesFile, header=FALSE, col.names="idToname", stringsAsFactors=FALSE)
    numRows <- nrow(Activities)
    
    ## read the activities reference file
    ActivitiesRef <- read.csv(ActivitiesReferenceFile, header = FALSE, sep=" ", col.names=c("id", "Activity"), stringsAsFactors=FALSE)
    
    ## replace the ids in Activities with names
    for(i in 1:numRows) 
    {
        #replacing with the name
        id = Activities[i,"idToname"]
        Activities[i,"idToname"] <- ActivitiesRef[id,"Activity"]
    }
    
    ####### SUBJECTS data ############################
    ## read the subject (subject_train.txt)
    Subjects <- read.csv(SubjectsFile, header=FALSE, col.names="subjects", stringsAsFactors=FALSE)
    
    ####### Features data ########################
    ## read the Features file and Create 2 vectors by searching for mean and std: 
    ## the first vector (fwf) to be used by read.fwf (fixed width file)
    ## the second vector (tidyFeaturesColNames) to be used to set the column names of the tidy data frame  
    FeaturesRef <- read.csv(FeaturesReferenceFile, header = FALSE, sep=" ", col.names=c("id", "Feature"), stringsAsFactors=FALSE)
    #### read the csv one row at a time (for loop). 
    ### if the text has mean or standard deviation then:
    ###       add rep(-1,15) to the fwf vector, else add -16 to the fwf vector
    ###       add the name to the tidyFeaturesColNames vector
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
      }
      else
      {
        ## first vector
        fwf <- c(fwf,-16)
        
        ## do not add to the 2nd vector
      }
    }
    
    ## read the Measurements file
    Measurements <- read.fwf(MeasurementsFile, header=FALSE, widths=fwf)

    
    ## check if all dimensions are equal
    if (nrow(Measurements) != numRows) return("ERROR -- Number of rows of Measurements File and Activities File DO NOT MATCH")
        
    
    ###### ADD all created columns to a data frame for Test and another for Train. 
    ## We need 2 data frames (testtidy and traintidy) because the number of rows are different, and cbind (called later) require same number of columns
    if (folder=="test")
    {
      # create a data frame that contains a column TestOrTrain filled with "Test"
      TestDF <- data.frame(TestOrTrain=rep("Test",numRows))
      # create testtidy that contains all Activities, Subject, and Measurements columns
      testtidy <- cbind(TestDF, Activities, Subjects, Measurements)
    }
    else
    {
      # create a data frame that contains a column TestOrTrain filled with "Train"
      TrainDF <- data.frame(TestOrTrain=rep("Train",numRows))
      # create testtidy that contains all Activities, Subject, and Measurements columns
      traintidy <- cbind(TrainDF, Activities, Subjects, Measurements)
    }
    
  }
  
  #### Merge the Test and Train data into one data set
  tidy <- rbind(testtidy,traintidy)
  
  #### Labels the data set with descriptive variable/column names
  colnames(tidy) <- c("Test_Or_Train", 
                      "Activity", 
                      "Subject",
                      tidyFeaturesColNames)
  
  ##### Create the first data set : TIDY.csv
  write.csv(tidy, "./TIDY.csv")
  print("TIDY.csv created successfully")

  ######### Second TIDY ###########
  # split the tidy data into a new variable Activity.Subject
  tidy2_1 <- split(tidy, list(factor(tidy$Activity), factor(tidy$Subject) )) 

  # calculate the mean for each column
  tidy2_2 <- sapply(tidy2_1, function(x) colMeans(x[,c(seq(4,ncol(tidy),1))])) 
  
  # create the 2nd data set as TIDY2.txt
  write.table(tidy2_2, "./TIDY2.txt", row.names=FALSE, sep="\t")
  print("TIDY2.txt created successfully")

  # EXTRA: create TIDY2 with row names
  write.csv(tidy2_2, "./TIDY2_withrows.csv")
  print("EXTRA: TIDY2_withrows.txt created successfully - With Row Names")
}