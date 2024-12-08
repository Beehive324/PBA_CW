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

#FEATURE CREATION 

"
- discretisizing the 'Income' feature into categories 'Very Low', 'Low', 'Medium', 'High', 'Very High'
- discretisizing the 'Age' feature into categories 'Young Adult', 'Adult', Middle Aged', 'Elder'
- dividing the current job years by experience to present a new feature for Employment Stability Index (ESI)
"

min_income <- min(completeLoanData$Income) #get the minimum income value 
max_income <- max(completeLoanData$Income) #get the maximum income value

completeLoanData$Income_Level <- cut( #cutting the income values into bins
  completeLoanData$Income, 
  breaks = c(min_income, 2000000, 4000000, 6000000, 8000000, max_income), #using minimum and maximum incomes to create 5 bins
  labels = c("Very Low", "Low", "Medium", "High", "Very High"), #labeling the 5 bins
  include.lowest = TRUE #including the minimum 
)

summary(completeLoanData$Income_Level)

#New Age Group Feature

min_age <- min(completeLoanData$Age) #get the minimum age value 
max_age <- max(completeLoanData$Age) #get the maximum age value

completeLoanData$Age_Group <- cut( #cutting the age values into bins
  completeLoanData$Age, 
  breaks = c(min_age, 25, 40, 60, max_age), #using minimum and maximum ages to create 5 bins
  labels = c("Young Adult", "Adult", "Middle Aged", "Elder"), #labeling the 4 bins
  include.lowest = TRUE #including the minimum 
)

summary(completeLoanData$Age_Group)

#Employment Stability Score/Index

completeLoanData$ESI <- completeLoanData$CURRENT_JOB_YRS / completeLoanData$Experience #ESI = current job years / experience
completeLoanData$ESI[is.na(completeLoanData$ESI)] <- 0 #replace N/A values with 0 

summary(completeLoanData$ESI)

#FEATURE ENCODING 

"
- encoding binary categorical features ('Married.Single' & 'Car_Ownership')
- ordinal encoding the 'House_Ownership' feature (since Owning is better than renting and renting is better than
than not renting or not owning in the context of a loan application)
- using One-Hot Encoding to handle the numerous categorical classes for 'Profession' and create a binary matrix 
that can be used during the modeling process
"

completeLoanData$Married.Single <- ifelse(completeLoanData$Married.Single == "married", 1, 0)
str(completeLoanData$Married.Single) #validating successful binary encoding for Married.Single 
summary(completeLoanData$Married.Single)

completeLoanData$Car_Ownership <- ifelse(completeLoanData$Car_Ownership == "yes", 1, 0)
str(completeLoanData$Car_Ownership) #validating successful binary encoding for Car_Ownership
summary(completeLoanData$Car_Ownership)

completeLoanData$House_Ownership <- ifelse(completeLoanData$House_Ownership == "owned", 2,
                                           ifelse(completeLoanData$House_Ownership == "rented", 1, 0))
str(completeLoanData$House_Ownership) #validating successful ordinal encoding for House_Ownership 
summary(completeLoanData$House_Ownership)

unique(completeLoanData$Profession)
professionOneHot <- model.matrix(~ Profession - 1, data = completeLoanData) #creating the binary matrix for Profession
completeLoanData <- cbind(completeLoanData, professionOneHot) #binding the matrix with the rest of the data
str(completeLoanData) #validating successful one-hot encoding for Profession 
head(completeLoanData) 



'FAIRSONS FEATURE CREATION WORK

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
'



