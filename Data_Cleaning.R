library(dplyr)

setwd('/Users/jiangyiran/Desktop/Purdue/2020Spring/546/project/covid-19')
data <- read.csv('./latestdata.csv')

dim(data)
colnames(data)

data$symptoms <- as.character(data$symptoms)
data$symptoms <- tolower(data$symptoms)
data$symptoms <- gsub("[0-9]", "", data$symptoms)

data <- data %>%
          filter(!(symptoms %in% c('','//')))


# View(data)
dim(data)

## Extract all Symptoms
symp <- as.character(data$symptoms)
symp_vec <- unlist(strsplit(symp,',|, |; |: |;|:'))
symp_vec <- gsub("[[:punct:]]", "", symp_vec)
symp_vec <- gsub("[0-9]", "", symp_vec)

## See all Unique Symptoms
unique(symp_vec)

## Check Specific Key words
symp_vec[grep('fever',symp_vec)]
data$symptoms[grep('fever',data$symptoms)]

## Extract gender data
data$sex_clean<-rep(NA,nrow(data))
data$sex_clean[grep('male',data$sex)]<-0
data$sex_clean[grep('female',data$sex)]<-1

## Extract Outcome data
outcome <- as.character(data$outcome)
outcome_vec <- unlist(strsplit(outcome,',|, |; |: |;|:'))
outcome_vec <- gsub("[[:punct:]]", "", outcome_vec)
outcome_vec <- gsub("[0-9]", "", outcome_vec)
as.data.frame(unique(outcome_vec))
data$outcome_clean<-rep(NA,nrow(data))
# 0-recovered 1-dead 2-ICU 3-Hospitalized
data$outcome_clean[grep('discharged|Discharged|recovered|discharge',data$outcome)]<-0
data$outcome_clean[grep('death|dead|died|Deceased',data$outcome)]<-1
data$outcome_clean[grep('treated in an intensive care unit',data$outcome)]<-2
data$outcome_clean[grep('Receiving Treatment|stable',data$outcome)]<-3

## Extract Age data
age<-data$age
as.data.frame(unique(age))
data$age_clean<-rep(NA,nrow(data))
# 0: 0-19 1:20-39 2:40-59 3:60+
age[grep('40-49|50-59',data$age)]<-50
age[grep('20-29|30-35|30-39',data$age)]<-30
age[grep('80-89|60-69|70-79|60-60|65-|90-99',data$age)]<-90
age[grep('0-6|0-10|13-19',data$age)]<-10
age<-as.numeric(as.character(age))
data$age_clean[age>-1&age<20]<-0
data$age_clean[age>20&age<40]<-1
data$age_clean[age>40&age<60]<-2
data$age_clean[age>60]<-3
