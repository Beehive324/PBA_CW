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



#Income classification

v_low <- min(completeLoanData$Income)
low <- quantile(completeLoanData$Income, 0.05)
median <- median(completeLoanData$Income)
high <- max(completeLoanData$Income, 0.95)
v_high <- max(completeLoanData$Income)

#create column called income category
completeLoanData$Income_Category <- ifelse(completeLoanData$Income <= v_low, "Very Low",
                           ifelse(completeLoanData$Income > v_low & completeLoanData$Income <= median, "Median",
                           ifelse(completeLoanData$Income > median & completeLoanData$Income <= high, "High",
                           ifelse(completeLoanData$Income > high & completeLoanData$Income <= v_high , "Very Hight", NA)

)))



#Age group category
completeLoanData$AgeGroup <- ifelse(completeLoanData$Age < 25, "Young Adult",

                            ifelse(completeLoanData$Age >= 25 & completeLoanData$Age <= 40, "Adult",

                            ifelse(completeLoanData$Age >= 41 & completeLoanData$Age <= 60, "Middle",

                            ifelse(completeLoanData$Age >= 60, "Elderly", NA)
                            
                            )))


#Employment Stability Score/Index
completeLoanData$ESI <- completeLoanData$CURRENT_JOB_YRS / completeLoanData$Experience




