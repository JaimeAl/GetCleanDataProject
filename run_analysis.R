
run_analysis <- function(){
        
        ##Reading all the files into R
        
        data_train <- read.table("./train/x_train.txt")
        subj_train <- read.table("./train/subject_train.txt")
        act_train <- read.table("./train/y_train.txt")
        
        data_test <- read.table("./test/x_test.txt")
        subj_test <- read.table("./test/subject_test.txt")
        act_test <- read.table("./test/y_test.txt")
        
        features <- read.table("features.txt")
        
        
        ##Join the subject and activity data to table so observations remain
        ## identifiable and traceable
        
        data_train <- mutate(data_train, subject = subj_train[,], activity = act_train[,])
        data_test <- mutate(data_test, subject = subj_test[,], activity = act_test[,])
        
        ## 1. Merges the training and the test sets to create one data set
        
        merged_dat <- merge (data_train, data_test, all = T)
        
        ## 2. Extracts only the measurements on the mean and standard deviation 
        ## for each measurement.
        ## I think it would be convenient to still stick to subject and act.
        ## cols 1 to 6 are the means and stds, by the way.
        
        merged_dat <- merged_dat [,c(1:6, 562,563)]
        
        ## 3. Uses descriptive activity names to name the activities in the data 
        ## set and 4. appropriately labels the data set with descriptive 
        ## variable names
        
        names(merged_dat) <- c(as.character(features[1:6, 2]), "subject", "activity")
        
        for(count in 1:10299){
                if(merged_dat[count, "activity"] == 1){merged_dat[count,"activity"] <- "walking"}
                if(merged_dat[count, "activity"] == 2){merged_dat[count,"activity"] <- "walking upstairs"}
                if(merged_dat[count, "activity"] == 3){merged_dat[count,"activity"] <- "walking downstairs"}
                if(merged_dat[count, "activity"] == 4){merged_dat[count,"activity"] <- "sitting"}
                if(merged_dat[count, "activity"] == 5){merged_dat[count,"activity"] <- "standing"}
                if(merged_dat[count, "activity"] == 6){merged_dat[count,"activity"] <- "laying"}
                
        }
        
        ## 5. From the data set in step 4, creates a second, independent tidy 
        ## data set with the average of each variable for each activity and each 
        ## subject.
        
        
        ## its pretty much done already since i put activities and subjects
        ## right from the start so ill just remove standar desviations
        
        merged_dat <- merged_dat[, c(1:3,7,8)]
        
        Result_from_course_project <<- merged_dat
        
}