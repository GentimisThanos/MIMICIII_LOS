library(tidyverse)
library(foreign)
data <- read.csv(file = "../Data/losPredict_V4.csv",header = TRUE, sep = ",") #Reads the data

##### Data preprocessing
data <- data[complete.cases(data),] #removes NA's if any
data <- data[-1] #removes rownum from SQL write
data <- within(data, rm("admittime","dischtime", "admission_type", "admission_location", "cpt_cd", "first_wardid", "last_wardid","dob")) #removes unwanted columns
colnames(data)[17] <- "Age"
data$full_los <- (data$full_los)/60/60/24 # converts full_los to days
data<-subset(data,data$full_los>0)
data<-subset(data,los>0)
data$Age<-data$Age/60/60/24/365
data<-subset(data,data$Age<100)

###### ONLY RUN FOLLOWING IF WANTING TO RUN DATA IN R NN

#str(data) checks the data structure...all need to be numeric for R to create NN
data$procedures_seq_num <- as.factor(data$procedures_seq_num) 
data$diagnoses_icd9_code <- as.numeric(data$diagnoses_icd9_code) 
data$procedures_icd9_code <- as.factor(data$procedures_icd9_code) 
data$religion <- as.numeric(data$religion)
data$marital_status <- as.numeric(data$marital_status)
data$ethnicity<- as.numeric(data$ethnicity)
data$costcenter <- as.numeric(data$costcenter)
data$diagnoses_icd9_code <- as.numeric(data$diagnoses_icd9_code)
data$last_careunit <- as.numeric(data$last_careunit)
data$procedures_seq_num <- as.numeric(data$procedures_seq_num)
data$procedures_icd9_code <- as.numeric(data$procedures_icd9_code)
data$prev_service <- as.numeric(data$prev_service)
data$curr_service <- as.numeric(data$curr_service)
data$insurance<-as.numeric(data$insurance)
data$first_careunit<-as.numeric(data$first_careunit)
data$gender<-as.numeric(data$gender)

### Loop to find highest number of procedures 
data$procedures<-rep(1,nrow(data))
data$procedures[1]=data$procedures_seq_num[1]

for (i in 2:nrow(data)) {
  ifelse (data$procedures_seq_num[i]<data$procedures_seq_num[i-1],data$procedures[i]<-data$procedures[i-1],data$procedures[i]<-data$procedures_seq_num[i])
}


data$diagnoses<-rep(1,nrow(data))
data$diagnoses[1]=data$diagnoses_sec_num[1]
for (i in 2:10) {
  ifelse (data$diagnoses_sec_num[i]<data$diagnoses_sec_num[i-1],data$diagnoses[i]<-data$diagnoses_sec_num[i],data$diagnoses[i]<-data$diagnoses[i-1])
}


#for (i in 1:nrow(data)) {
#if (data$diagnoses_icd9_code[i]>99) {data$diagnoses_icd9_code[i]<-floor(data$diagnoses_icd9_code[i]/10)}}

data<-subset(data,data$diagnoses_icd9_code>99)


data<- within(data, rm("procedures_seq_num","prev_service","subject_id","hadm_id"))

data<-unique(data)

write.csv(data, file = "../Results/Clean_LOS_1.csv")
