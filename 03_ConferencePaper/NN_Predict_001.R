## Install Packages
#install.packages("neuralnet")
#install.packages("tictoc")
#install.packages("caret")

##################################### PREAMBLE ###############################################

#setwd("D:/HealthInformatics/DataAnalytics/Active_Students/Alex_Durante/MIMIC/MIMIC_R/LOS_Predict")

## MAKE SURE CSV FILE IS IN SAME FOLDER AS CODES
#data_file_name <- "lOsPredict.csv" #change this line to match your csv file
total_data_points_to_use <- 1000 # change this line to chop the data set to as many rows as you want
percent_data_train <- 0.75 # this is the percentage of the data set that will be used for training

## ENSURE COLUMN 1 IS OUTPUT COLUMN
output_column_name <- "los" # change this to match the name of the output column
input_columns <- 2:7 #define the number of the input columns

## Import Libraries
library(neuralnet)
library(tictoc)
library(caret)

## Reading from the csv file and cleaning up the empty spots
data <- read.csv(file = "losPredict.csv",header = TRUE, sep = ",")
data <- data[-1]
data <- data[-1]
data <- head(data,total_data_points_to_use)
data <- data[complete.cases(data),]

## OH GOD WHY
data$ethnicity <- as.numeric(data$ethnicity)
data$insurance <- as.numeric(data$insurance)
data$religion <- as.numeric(data$religion)
data$marital_status <- as.numeric(data$marital_status)
data$gender <- as.numeric(data$gender)

## Split the data into a Training set and a Testing set
index <- sample(1:nrow(data),round(percent_data_train*nrow(data)))
train <- data[index,]
test <- data[-index,]

## Find the Maximum and Minumum in the data set
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

## Scale the data set using "maxs" and "mins"
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

## Create scaled Training data set and scaled Testing data set
train_s <- scaled[index,]
test_s <- scaled[-index,]

## Assign the names column to a variable
n <- names(train_s)

## Train nueral net
f <- as.formula(paste(paste(output_column_name," ~"), paste(n[!n %in% output_column_name], collapse = " + ")))
tic()
nn <- neuralnet(f,data=train_s,hidden=c(5,3),linear.output=T, rep = 3)
toc()
## Plot the nueral net
plotOutput <- plot(nn)

## Predict the test data set with the nueral net
pr_nn <- compute(nn,test_s[,input_columns])


## Create bins of less than 0.2 and more than 0.2

pr_nn_2=ifelse(pr_nn$net.result >0.2,1,0)
test_r_2=ifelse(test_s[1]>0.2,1,0)

## Scale the results of the nural net prediction
pr_nn_s <- pr_nn$net.result*(max(data[1])-min(data[1]))+min(data[1])
test_r <- (test_s[1])*(max(data[1])-min(data[1]))+min(data[1])
test_r_f <- as.factor(test_r[[1]])
pr_nn_s_r <- as.factor(round(pr_nn_s))

## Scale the results of the nueral net prediction
pr_nn_s <- pr_nn$net.result*(max(data[1])-min(data[1]))+min(data[1])
test_r <- (test_s[1])*(max(data[1])-min(data[1]))+min(data[1])
test_r_f <- as.factor(test_r[[1]])

## Use MSE to check the accuracy of the nural net prediction
MSE_nn <- sqrt(sum((test_r - pr_nn_s)^2))/nrow(test_r)
STE_nn <- sum(round(test_r - pr_nn_s))/nrow(test_r)

## Compare the MSE's of the linear model and the nueral net
paste("The MSE of the NN is: ",MSE_nn)
paste("The STE of the NN is: ",STE_nn)


## Creates the confusion matrix for less than 0.2 and more than 0.2
confusion <- table(pr_nn_2, test_r_2)


save(nn, file = "nn.rda")
confusion
