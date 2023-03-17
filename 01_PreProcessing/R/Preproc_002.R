library(plyr)
library(foreign)


## MAKE SURE CSV FILE IS IN SAME FOLDER AS CODES


data <- read.csv(file = "../Data/losPredict_V3.csv",header = TRUE, sep = ",") # change this line to match your csv file

##### Data preprocessing
data <- data[complete.cases(data),] #removes NA's if any
data <- data[-1] #removes rownum from SQL write
data <- within(data, rm("admittime","dischtime", "admission_type", "admission_location", "cpt_cd", "first_wardid", "last_wardid","dob")) #removes unwanted columns
colnames(data)[18] <- "Age"
data$full_los <- (data$full_los)/60/60/24 # converts full_los to days
data<-subset(data,data$full_los>0)
data<-subset(data,los>0)
data$Age<-data$Age/60/60/24/365
data<-subset(data,data$Age<100)
data<-subset(data,data$full_los>=data$los)
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
data$procedures<-rep(1,nrow(data))
data$procedures[1]=data$procedures_seq_num[1]

for (i in 2:nrow(data)) {
  ifelse (data$procedures_seq_num[i]<data$procedures_seq_num[i-1],data$procedures[i]<-data$procedures[i-1],data$procedures[i]<-data$procedures_seq_num[i])
}
data<-subset(data,data$diagnoses_icd9_code>99)
data1 <- read.csv(file = "dpa1.csv",header = TRUE, sep = ",") # change this line to match your csv file
colnames(data1)[1]<-"hadm_id"
data=merge(data,data1,by="hadm_id")
colnames(data)[21]<-"diagnoses_count"
data1<- within(data, rm("procedures_seq_num","subject_id","procedures_icd9_code","diagnoses_seq_num"))
data1<-unique(data1)
write.csv(data1, file = "../Data/Clean_LOS_1.csv")