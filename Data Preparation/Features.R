

#Loading the entire dataset
completeLoanData <- read.csv('completeLoanData.csv', na.strings = c("NA", ""))

#Checking the data type of every feature 
#str(completeLoanData)

# handling missing values
#summary(completeLoanData)


LoanData_NA_count <- apply(is.na(completeLoanData), 2, sum)
LonData_NA_perc <- LoanData_NA_count / dim(completeLoanData)[1] * 100
LonData_NA_perc

#preparing training/test set
n_rows <- nrow(completeLoanData)
training_idx <- sample(n_rows, n_rows * 0.7)
training_data <- completeLoanData[training_idx,]
test_data <- completeLoanData[-training_idx,]

#data for training data
write.csv(training_data,
        file = "training_data.csv",
        row.names = TRUE,
        na = "",
        quote = TRUE)


#data for test data
write.csv(test_data,
        file = "test_data.csv",
        row.names = TRUE,
        na = "",
        quote = TRUE)




#feature creation

#Income classification
#very low
v_low <- min(completeLoanData$Income)

#low
low <- quantile(completeLoanData$Income, 0.05)

#median
median <- median(completeLoanData$Income)

#high
high <- max(completeLoanData$Income, 0.95)

#very high
v_high <- max(completeLoanData$Income)





#create column called income category

completeLoanData$Income_Category <- ifelse(completeLoanData$Income <= v_low, "Very Low",
                           ifelse(completeLoanData$Income > v_low & completeLoanData$Income <= median, "Median",
                           ifelse(completeLoanData$Income > median & completeLoanData$Income <= high, "High",
                           ifelse(completeLoanData$Income > high & completeLoanData$Income <= v_high , "Very Hight", NA)

)))

#Employment Stability Score/Index

#Age group category
completeLoanData$AgeGroup <- ifelse(completeLoanData$Age < 25, "Young Adult",

                            ifelse(completeLoanData$Age >= 25 & completeLoanData$Age <= 40, "Adult",

                            ifelse(completeLoanData$Age >= 41 & completeLoanData$Age <= 60, "Middle",

                            ifelse(completeLoanData$Age >= 60, "Elderly", NA)
                            
                            )))



completeLoanData$ESI <- completeLoanData$CURRENT_JOB_YRS / completeLoanData$Experience




