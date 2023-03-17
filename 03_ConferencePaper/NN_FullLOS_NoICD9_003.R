## Loads appropriate libraries
library(neuralnet)
library(caret)
library(e1071)
library(ggplot2)
library(tictoc)


total_data_points_to_use <- 500# Change this line to chop the data set to as many rows as you want
percent_data_train <- 0.9 # sets the percentage of the data set that will be used for training
maximum_days<-100 # Sets the maximum number of days for the LOS
k=3 # Sets the number of repetitions for the nn

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

## Choses only the cases where the diagnoses sequence number is 1
#data <- subset(data, diagnoses_seq_num <2)
#data <- subset(data, select = -diagnoses_seq_num )

## Moves the output variable first
col_idx <- grep("full_los", names(data)) # Creates an index with the chosen variable as first
data<- data[, c(col_idx, (1:ncol(data))[-col_idx])] # Rearranges the data frame
input_columns <- 2:ncol(data) # Defines the number of the input columns to be all but the first column
## Initiates vector of zeroes for the accuracy
Acc  <- rep(0,k)
data <- data[sample(nrow(data),size = total_data_points_to_use), ] # Selects the desired ammount of rows at random
## Finds the Maximum and Minumum in the data set for each column
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)

## Scales the data set using "maxs" and "mins" for each column to [0,1] range
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins)) 

for (i in 1:k){
  ## Splits the data into a Training set and a Test set
  index <- sample(1:nrow(scaled),round(percent_data_train*nrow(scaled))) # chooses a set of rows at random 
  
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
write.csv(Acc, file = paste0("../Results/Accuracy1_",total_data_points_to_use,"_",maximum_days,"_",k, ".csv"))

