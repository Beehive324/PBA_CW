"""

Data Preprocessing

Sections 1 and Sections 2
"""

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


str(training_data)
str(test_data)

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
