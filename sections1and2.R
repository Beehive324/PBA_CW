#-------------------------------------------------------------------------------

#SECTION 1 - DATA INSPECTION

#Loading the entire dataset
completeLoanData <- read.csv('completeLoanData.csv')

#Checking the data type of every feature 
str(completeLoanData)

#-------------------------------------------------------------------------------

#SECTION 2 - DATA UNDERSTANDING, PRE-PROCESSING AND EXPLORATION 


#FEATURE DELETION

"
- dropping 'Id' feature due to its irrelevance to our problem statement
- dropping 'CITY' feature to reduce noise and simplify data
"
completeLoanData <- completeLoanData[, !names(completeLoanData) %in% c("Id", "CITY")]

#EXPLORATORY DATA ANALYSIS AND DETECTED ERRORS 

"
- checking the dataset for missing field values 
- checking the categorical features with string data types for possible duplicate values and correcting them accordingly. 
- checking the 'Income' feature for outliers 
"
loan_data_empty_count <- apply(completeLoanData == "" | is.na(completeLoanData),2,sum)
loan_data_empty_count


unique_marriage <- unique(completeLoanData$Married.Single)
unique_marriage

unique_house_ownership <- unique(completeLoanData$House_Ownership)
unique_house_ownership

unique_car_ownership <- unique(completeLoanData$Car_Ownership)
unique_car_ownership

unique_profession <- unique(completeLoanData$Profession)
unique_profession

unique_states <- unique(completeLoanData$STATE)
unique_states
library(stringr)
completeLoanData$STATE <- trimws(completeLoanData$STATE)
completeLoanData$STATE <- str_replace_all(completeLoanData$STATE, "Uttar_Pradesh\\[5\\]", "Uttar_Pradesh")
unique_states

summary(completeLoanData$Income)

hist(completeLoanData$Income, 
     main = "Income Distribution", 
     xlab = "Income", 
     ylab = "Frequency",
     col="darkgreen",
     )

dev.off()

boxplot(completeLoanData$Income, 
        main = "Income Distribution", 
        xlab = "Income", 
        ylab = "Values", 
        col = "darkorange", 
        horizontal = TRUE)

dev.off()

plot(density(completeLoanData$Income), 
     main = "Income Distribution", 
     xlab = "Income", 
     ylab = "Density", 
     col = "blue", 
     lwd = 1.5) 

dev.off()






































