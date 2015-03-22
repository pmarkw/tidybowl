The run_analysis.R script in this repository processes data files from the
Human Activity Recognition Using Smartphones Data Set 
at http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Download and unzip these files into folder "UCI HAR Dataset" in your working directory.
Place the run_analysis.R script in your working directory.
Edit the setwd command at the top of the script and then run it.

This script combines activity names, subject identifiers, and measured observations from individual tests
for both the "training" data files and the "testing" data files.
It then combines training and testing data into one data set and extracts only mean and standard deviation data.

Finally it produces a summary data set showing the average mean and average std values 
for each activity and test subject.


