run_analysis <- function() {
        
        testdata <- read.csv("../UCI HAR Dataset//test/X_test.txt", sep = "", header = FALSE)
        test_subjects <- readLines("../UCI HAR Dataset/test/subject_test.txt")
        test_activity <- readLines("../UCI HAR Dataset/test/y_test.txt")
        
        traindata <- read.csv("../UCI HAR Dataset//train/X_train.txt", sep = "", header = FALSE)
        train_subjects <- readLines("../UCI HAR Dataset/train/subject_train.txt")
        train_activity <- readLines("../UCI HAR Dataset/train/y_train.txt")
        
        cnames <- readLines("../UCI HAR Dataset/features.txt")
        names(testdata) <- cnames
        names(traindata) <- cnames
        
        cn <- grep("-mean|-std", cnames)
        testdata <- testdata[ ,cn]
        traindata <- traindata[ ,cn]
        
        testdf <- cbind(test_subjects, test_activity, testdata)
        testdf$test_subjects <- as.character(testdf$test_subjects)
        testdf$test_activity <- as.character(testdf$test_activity)
        names(testdf)[1:2] <- c("subjects", "activity")
        
        traindf <- cbind(train_subjects, train_activity, traindata)
        traindf$train_subjects <- as.character(traindf$train_subjects)
        traindf$train_activity <- as.character(traindf$train_activity)
        names(traindf)[1:2] <- c("subjects", "activity")
        
        df <- rbind(traindf, testdf)
        
        # Descriptive activity names
        vc <- df$activity
        for (i in 1:length(vc)) {
                if (vc[i] == "1") { 
                        vc[i] <- "Walking"
                } else if (vc[i] == "2") {
                        vc[i] <- "Walking.Upstairs"
                } else if (vc[i] == "3") {
                        vc[i] <- "Walking.Downstairs"
                } else if (vc[i] == "4") {
                        vc[i] <- "Sitting"
                } else if (vc[i] == "5") {
                        vc[i] <- "Standing"
                } else if (vc[i] == "6") 
                        vc[i] <- "Laying"
        }
        df$activity <- vc
        
        # Label data set with descriptive variable names
        cn <- names(df[3:81])
        for (i in 1:length(cn))
               cn[i] <- strsplit(cn[i], split = " ")[[1]][2]
       
        names(df) <- c(names(df[1:2]), cn)
        
        df$subjects <- as.numeric(df$subjects)
        df <- df[order(df$subjects),]
        write.csv(df, "data.txt", row.names = FALSE)
        
        
}