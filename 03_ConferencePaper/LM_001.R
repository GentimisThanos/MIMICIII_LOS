## Loads appropriate libraries
library(easypackages)
libraries("neuralnet","caret","e1071","tidyverse","tictoc")

## MAKE SURE THE CSV FILE IS IN THE FOLDER DATA AND THE WORKING DIRECTORY IS THE SAME AS THE CODE
total_data_points_to_use <- 30000# Change this line to chop the data set to as many rows as you want
percent_data_train <- 0.9 # sets the percentage of the data set that will be used for training
maximum_days<-100 # Sets the maximum number of days for the LOS
k=150 # Sets the number of repetitions 

## Reads from the csv file, 
data <- read.csv(file = "../Data/Clean_LOS_1.csv",header = TRUE, sep = ",") # Reads from csv
data <- data[-1] # Removes the first dummy column
data<-within(data,rm("hadm_id","diagnoses_icd9_code"))

## Selects the columns from the data to by used, 
## Removing a number removes the corresponding column, update if more than 10 columns used
mycolumns<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) 

## Defines input column
output_column_name <- "full_los" # change this to match the name of the output column

## Cleans up the empty spots keeping only LOS less than the set max
data <- data[complete.cases(data),] # Keeps only full rows
data <- subset(data, data$full_los<maximum_days) # Removes the entries with LOS more than the set maximum days days
data<- data[mycolumns] # Keeps only our selected columns

## Moves the output variable first
col_idx <- grep("full_los", names(data)) # Creates an index with the chosen variable as first
data<- data[, c(col_idx, (1:ncol(data))[-col_idx])] # Rearranges the data frame
input_columns <- 2:ncol(data) # Defines the number of the input columns to be all but the first column
accuracies=rep(0,k) #creates a list of zeroes to save the accuracies  
data <- data[sample(nrow(data),size = total_data_points_to_use), ] # Selects the desired amount of rows at random

for (i in 1:k){
## Splits the data into a Training set and a Test set
index <- sample(1:nrow(data),round(percent_data_train*nrow(data))) # chooses a set of rows at random 
train <- data[index,] # creates the training set
test <- data[-index,] # creates the test set

## Finds the Maximum and Minumum in the data set for each column
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)

## Scales the data set using "maxs" and "mins" for each column to [0,1] range
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins)) 

## Creates a scaled Training data set and a scaled Testing data set
train_s <- scaled[index,]
test_s <- scaled[-index,]

## Assigns the row of names to a variable
n <- names(data)

## Creates a naming variable for the LM using a specific syntax
f <- as.formula(paste(paste(output_column_name," ~"), paste(n[!n %in% output_column_name], collapse = " + "))) 

fit<-lm(f,data=data)
plot(fit$residuals)
pr_lm <- predict(fit,test_s)

## Creates bins of less than 5 days (0.25) and more than 5 days (0.25) for the prediction and the actual test set
pr_lm_2=ifelse(pr_lm >(5/maximum_days),1,0)
test_r_2=ifelse(test_s[1]>(5/maximum_days),1,0)

## Creates the confusion matrix for less than 5 days and more than 5 days
confusion <- table(pr_lm_2, test_r_2)

## Computes the accuracy of prediction
accuracies[i]=(confusion[1,1]+confusion[2,2])/sum(confusion)
}
write.csv(accuracies,"../Results/AccuraciesLM.csv")
