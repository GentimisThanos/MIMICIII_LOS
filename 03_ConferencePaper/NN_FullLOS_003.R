## Loads appropriate libraries
library(easypackages)
libraries("neuralnet","caret","e1071","tidyverse","tictoc")

## MAKE SURE THE CSV FILE IS IN THE FOLDER DATA AND THE WORKING DIRECTORY IS THE SAME AS THE CODE
total_data_points_to_use <- 3000# Change this line to chop the data set to as many rows as you want (use on slower machines)
percent_data_train <- 0.9 # sets the percentage of the data set that will be used for training
k=150 # Sets the number of repetitions for the nn

## Reads from the csv file, 
data <- read.csv(file = "../Data/YOURDATAHERE.csv",header = TRUE, sep = ",") # Reads from csv
data <- data[-1] # Removes the first dummy column

## Selects the columns from the data to by used, 
## Removing a number removes the corresponding column, update if more than 10 columns used


## Defines input column
output_column_name <- "MYVARIABLEOFINTEREST" # change this to match the name of the output column you are trying to predict
data <- data[complete.cases(data),] # Keeps only full rows


## Moves the output variable first
col_idx <- grep(output_column_name, names(data)) # Creates an index with the chosen variable as first
data<- data[, c(col_idx, (1:ncol(data))[-col_idx])] # Rearranges the data frame
input_columns <- 2:ncol(data) # Defines the number of the input columns to be all but the first column
accuracies=rep(0,k) # Creates a vector f zeroes to store the accuracies
data <- data[sample(nrow(data),size = total_data_points_to_use), ] # Selects the desired amount of rows at random

## Finds the Maximum and Minumum in the data set for each column
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)

## Scales the data set using "maxs" and "mins" for each column to [0,1] range
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins)) 

## Start of loop
for (i in 1:k){

## Creates a scaled Training data set and a scaled Testing data set
train_s <- scaled[index,]
test_s <- scaled[-index,]

## Assigns the row of names to a variable
n <- names(data)

## Creates a naming variable for the neural nets using a specific syntax
f <- as.formula(paste(paste(output_column_name," ~"), paste(n[!n %in% output_column_name], collapse = " + "))) 

## Creates a neural network with 2 layers and 5/3 nodes using the scaled training set breaks after 13 fails
tic() # Starts time counting
repeat{
  
  nn <- neuralnet(f,data=train_s,hidden=c(5,3),linear.output=T, threshold = 0.01, lifesign = "minimal", err.fct = "sse", act.fct = "logistic", algorithm = "rprop+")
  
  if (length(nn) == 13) break
}
toc()# Stops time counting

#save(nn, file = paste0("nn_",total_data_points_to_use,"_",maximum_days,"_",i,".rda")) #Saves the nn
## Creates a predictions column using the scaled test set
pr_nn <- compute(nn,test_s[,input_columns])

## Creates bins of less than 5 days (0.25) and more than 5 days (0.25) for the prediction and the actual test set
pr_nn_2=ifelse(pr_nn$net.result >(5/maximum_days),1,0)
test_r_2=ifelse(test_s[1]>(5/maximum_days),1,0)

## Creates the confusion matrix for less than 5 days and more than 5 days
confusion <- table(pr_nn_2, test_r_2)

## Computes the accuracy of prediction
acc=(confusion[1,1]+confusion[2,2])/sum(confusion)


Acc[[i]]=acc

}

## Creates a csv with the accuracies
write.csv(Acc, file = paste0("Accuracy_",total_data_points_to_use,"_",maximum_days,"_",k, ".csv"))



