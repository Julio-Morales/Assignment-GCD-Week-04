 # 0. Libraries.
 # 
 
 library(reshape2)
 library(dplyr)

 # 1. Getting data from source.
 #
 # According to assignment, data can be obtained from the following URL in zip 
 # format.
 #

 zipfileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
 zfile <- "./Data/Dataset.zip"
 
 # 1.a Preparing to extract files.
 # 
 # Data is stored in directory "./Data". If it does not exist, it would be 
 # created.
 # All zipped files will be extracted into "./Data" directory.
 
 if(!file.exists(".Data")) {dir.create("./Data")}
 download.file(zipfileUrl,destfile = zfile, method = "curl")
 unzip(zfile,exdir = "./Data")
 
 # 1.b Loading files into R tables.
 # 
 # Information is stored at "./Data/UCI HAR Dataset" and is distributed into the
 # following files or directories (this information and more can be consulted at
 # README.txt):
 #
 # - 'README.txt'
 # - 'features_info.txt': Shows information about the variables used on the 
 #    feature vector.
 # - 'features.txt': List of all features.
 # - 'activity_labels.txt': Links the class labels with their activity name.
 # - 'train/X_train.txt': Training set.
 # - 'train/y_train.txt': Training labels.
 # - 'test/X_test.txt': Test set.
 # - 'test/y_test.txt': Test labels.
 # The following files are available for the train and test data. Their 
 # descriptions are equivalent. 
 # - 'train/subject_train.txt': Each row identifies the subject who performed
 #   the activity for each window sample. Its range is from 1 to 30. 
 # 
 # Assignment requires to work with mean and standard deviation from
 # observations stored at training and test sets. Therefore, Inertial 
 # information is not necessary to complete this job.
 #

 Activity <- read.table("./Data/UCI HAR Dataset/activity_labels.txt",sep = " ",stringsAsFactors = TRUE)
 Features <- read.table("./Data/UCI HAR Dataset/features.txt",sep = " ",stringsAsFactors = TRUE)
 
 TrainingDataset <- read.table("./Data/UCI HAR Dataset/train/X_train.txt",header = FALSE)
 TrainingLabel <- read.table("./Data/UCI HAR Dataset/train/y_train.txt",sep = " ",stringsAsFactors = TRUE)
 TrainingSubject <- read.table("./Data/UCI HAR Dataset/train/subject_train.txt",sep = " ",stringsAsFactors = TRUE)
 
 TestDataset <- read.table("./Data/UCI HAR Dataset/test/X_test.txt",header = FALSE)
 TestLabel <- read.table("./Data/UCI HAR Dataset/test/y_test.txt",sep = " ",stringsAsFactors = TRUE)
 TestSubject <- read.table("./Data/UCI HAR Dataset/test/subject_test.txt",sep = " ",stringsAsFactors = TRUE)
 
 # Naming headers
 
 names(Activity) <- c("ID","Activity")

 names(TrainingDataset) <- Features$V2
 names(TrainingLabel) <- "Activity"
 names(TrainingSubject) <- "Subject"
 
 names(TestDataset) <- Features$V2
 names(TestLabel) <- "Activity"
 names(TestSubject) <- "Subject"
 
 # 2. Merging Data.
 # 
 # Activity is a table with 6 rows and 2 columns. First column is a numeric ID for each 
 # human activity tested and second one is its description.
 # Features has 561 rows and 2 columns. Second column contains the 
 # header for each Training or Test Dataset.
 #
 # TrainingDataset is a table with 7352 rows and 561 columns. Each column 
 # corresponds to each variable defined at Features.
 # TrainingLabel has 7352 rows and 1 column. Their values vary between 1 and 6.
 # TrainingSubject has 7352 rows and 1 column. Their values vary between 1 and 
 # 30.
 #
 # For Test tables are similar with Training ones with the difference that 
 # number of observations (rows) are 2947.
 #
 # To merge this tables, training variables need to be binded by columns in a 
 # new dataset called MergeTraining. Test variables also are binded by columns 
 # and stored in MergeTest.
 #
 # Finally, MergeTraining and MergeTest can be binded by rows in a MergeDataset 
 # table. Observe that MergeDataset has no descriptive headers and activity is
 # identified by its ID number.
 #
 
 MergeTraining <- cbind(TrainingSubject,TrainingLabel,TrainingDataset)
 MergeTest <- cbind(TestSubject, TestLabel, TestDataset)
 
 MergeDataset <- rbind(MergeTraining, MergeTest)
 
 # 3. Identifying headers and activities.
 #
 # MergeDataset has its activity ID in column 2 and need to be change for its
 # descriptive name.
 # Combining MergeDataset[,2] with Activity[,1] by using merge() into a temp 
 # variable, a descriptive activity can be obtained in Dataset.
 # 
 # Merge() function reorders columns and rows, therefore, sort argument must be 
 # set into FALSE if we want to compare inputs with output.
 # Also, we need to reorder columns to have descriptive activity at the begining
 # of the Dataset.
 #
 
 temp <- merge(MergeDataset,Activity,by.x = 2,by.y = 1,all = TRUE, sort = FALSE)
 temp <- temp[,c(2,564,3:563)]
 names(temp)[2] <- "Activity" # After merging header results in "Activity.y"

 # 4. Filtering mean and standard deviation measurements.
 #
 # To extract mean and standard deviation, grep() function with a regular
 # expression with "mean(" or "std(" help us to find these patterns from 
 # names() of temp variable.
 # "(" is a reserved character in regular expresion. Therefore, it must be
 # parsed as "\\(" in order to match this pattern.
 #
 
 FilteredDataset <- temp[,grep("Subject|Activity|mean\\(|std\\(", names(temp))]
 
 # FilteredDataset is a table with 10299 rows and 68 columns.
 
 # 5. Tidy Dataset.
 # 
 # In order to obtain an independent tidy data set with the average of each 
 # variable for each activity and each subject, FilteredDataset must be 
 # rearranged in order to have 63 variables as one measurement by using melt()
 # function. This new table is called "DatasetMelt".
 #
 # To calculate the average of each variable for each activity and each subject,
 # dcast() function can be used on DatasetMelt by recasting Activity and 
 # Subject in the rows, variable in the columns and taking the mean for each
 # value. DatasetCast contains in a easy way to visualize the requirement.
 #
 # However, DatasetCast is not a tidy dataset because it has observations as a 
 # headers. Therefore, DatasetCast must be melted in a similar way as 
 # FilteredDataset in order to obtain a tidy dataset.
 #
 
 
 IdMelt <- names(FilteredDataset)[1:2]
 variables <- names(FilteredDataset)[3:68]
 DatasetMelt <- melt(FilteredDataset,id= IdMelt, measure.vars = variables)
 
 #
 # DatasetMelt is a table with 679.734 rows and 4 columns. These columns are:
 # Subject.
 # Activity.
 # Variable: contains 66 mean() and std() headers.
 # Value: readings from each subject, activity and measurement.
 #
 
 DatasetCast <- dcast(DatasetMelt, Activity + Subject ~ variable, mean)
 View(DatasetCast)
 
 HumanActivityMean <- melt(DatasetCast,id= IdMelt, measure.vars = variables)
 View(HumanActivityMean)
 
 write.table(HumanActivityMean, file = "HumanActivityMean.txt", row.names = FALSE)
 
 #
 # HumanActivityMean is a table with 11.880 rows and 4 columns. Each row
 # corresponds to one observation of a subject per activity, measurement name
 # (variable) and its mean value.
 #
 
 