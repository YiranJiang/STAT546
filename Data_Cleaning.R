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



