This project contains:
* run_analysis.R: the only script needed to produce 2 data sets:
	- TIDY.csv : 1st dataset that integrates Test data with Train data. 
 	- TIDY2.csv: 2nd dataset (step 5 of the assignment) that provides the average of each variable for each activity and each subject. 

(extra: I created TIDY2_withrows.csv that contains the list of row names)

see CodeBook.md for more details on the columns

To run the script: copy the script to a local working directory that also contains the Samsung data.
then run these commands:
source("run_analysis.R")
run_analysis()

The script run_analysis.R has comments to describe how the code was wrritten.
Summary:
=========
* I used the same code to loop thru the test data set and the train data set. I used a loop to iterate on both folders
* Activity vector: I created a vector by reading the Activities file and changed the id to a name using activity_label as reference. 
* Subjects vector: I read the Subjects file into a vector
* Features Reference file: I read features.txt into a vector. using this file I created another 2 vectors: the first vector (fwf) to be used by read.fwf (fixed width file) to read the measurements file. The second vector (tidyFeaturesColNames) to be used to set the column names of the tidy data frame.  
* Measurements: I read the X_*.txt files into a vector
* for each folder, I created a data frame. testtidy for the test folder, and traintidy for the train folder
* I used rbind to bind testtidy and traintidy
* I renamed the column names for tidy
* I created a csv file from the tidy dataframe
* to create the 2nd data set (step 5). I first split the tidy data set by activity and subject, then I used colMeans to calcualte the mean for each feature.
* I wrote the 2nd data set into a txt file.
