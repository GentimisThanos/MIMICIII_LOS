library(doParallel)
library(kernlab)
library(ROCR)
library(tictoc)
library(ggplot2)
library(caret)
source("./Functions/cleandata.R")
source("./Functions/bestsigc.R")
source("./Functions/finmatrix.R")
## ================================= Initial Parameters to set up ===================================================
total_data_points_to_use = 30000# Change this line to chop the data set to as many rows as you want
percent_data_train = 0.9 # sets the percentage of the data set that will be used for training
maximum_days = 100 # Sets the maximum number of days for the LOS
cut_off_day = 5 # Sets the threshold for long vs short stay
max_order_of_mag_sigma <- -1 # Set the maximum order of magnitude for sigma
min_order_of_mag_sigma <- -5 # Set the minimum order of magnitude for sigma
total_errors_to_test <- 20 # set number of different errors to test
error_step <- 10 # set base of power in the error step
## ================================= Cleans data, finds best c and sigma using functions ===========================
data= cleandata(total_data_points_to_use,maximum_days,cut_off_day,percent_data_train) # Utilizes the preprocessing 
# function cleandata to create a data set of our specifications
index <- sample(1:nrow(data),round(percent_data_train*nrow(data))) # samples a percent of the data for training
traindata=data[index,] # Saves the training set 
testdata=data[-index,] # Saves the test set
input_columns=c(2:ncol(traindata)) # Tells R which columns to use as input.
output=bestsigc(traindata,min_order_of_mag_sigma,max_order_of_mag_sigma,total_errors_to_test,error_step) # Calls 
# the function bestsig to find the best c and sigmas
sig=10^(output[1]-1) # computes the sigma
erc=error_step^(output[2]) # computes the c
#================================== Main algorithm =============================
tic() # starts the clock
svp <- ksvm(as.matrix(traindata[,input_columns]),traindata[,1],type="C-svc",kernel="rbfdot", kpar = list(sigma = sig), C=erc,scaled=c())  
# computes the suppor vector machine using the original training set.the first argument is the input columns
# the second argument is the output column for training, the rest are settings for the svm ie, using c error type the RBF kernel and
# a figed sigma and C without scalling
 # Utilizes the preprocessing 
# function cleandata to get the 
pred_data=as.factor(predict(svp,testdata[,input_columns]))
con_mat <- confusionMatrix(pred_data,testdata[,1])
con_mat
toc()



