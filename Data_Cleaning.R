library(dplyr)

data <- read.csv('/Users/wang/Desktop/546 Project/latestdata.csv')

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
as.data.frame(unique(symp_vec))

## Check Specific Key words
symp_vec[grep('fever',symp_vec)]
data$symptoms[grep('fever',data$symptoms)]

## Add new column for clean data
data$symptoms_clean <- rep("",nrow(data))

data$symptoms_clean[grep('rever|febre|fever|feverish',data$symptoms)] <- "Fever"
data$symptoms_clean[grep('cough|couh|throat|throa|sputum|phlegm|pharyngitis|pharyngeal|expectoration|tosse',data$symptoms)] <- lapply(data$symptoms_clean[grep('cough|couh|throat|throa|sputum|phlegm|pharyngitis|pharyngeal|expectoration|tosse',data$symptoms)], function(x) paste("Cough",x,sep=","))
data$symptoms_clean[grep('respirat|breathing|dyspnea|dsypnea|breath|tight|anhelation|distress|wheezing|gasp|grasp|hypoxia',data$symptoms)] <- lapply(data$symptoms_clean[grep('respirat|breathing|dyspnea|dsypnea|breath|tight|anhelation|distress|wheezing|gasp|grasp|hypoxia',data$symptoms)], function(x) paste("Dyspnea",x,sep=","))
data$symptoms_clean[grep('pain|ach|body|limbs|muscle|muscular|myalgia|mialgia',data$symptoms)] <- lapply(data$symptoms_clean[grep('pain|ach|body|limbs|muscle|muscular|myalgia|mialgia',data$symptoms)], function(x) paste("Body Pain",x,sep=","))
data$symptoms_clean[grep('anorexia|nausea|mausea|esophageal|vomit|inappetance|inapatence|pharyngeal|pharynx|emesis|dysphagia',data$symptoms)] <- lapply(data$symptoms_clean[grep('anorexia|nausea|mausea|esophageal|vomit|inappetance|inapatence|pharyngeal|pharynx|emesis|dysphagia',data$symptoms)], function(x) paste("Anorexia",x,sep=","))
data$symptoms_clean[grep('nasal|nose|sneez|rhinorrhea|rhinorrhoea|coriza',data$symptoms)] <- lapply(data$symptoms_clean[grep('nasal|nose|sneez|rhinorrhea|rhinorrhoea|coriza',data$symptoms)], function(x) paste("Rhinorrhea",x,sep=","))
data$symptoms_clean[grep('cold|chill',data$symptoms)] <- lapply(data$symptoms_clean[grep('cold|chill',data$symptoms)], function(x) paste("Chills",x,sep=","))
data$symptoms_clean[grep('diarrhea|diarrhoea|diarrheoa',data$symptoms)] <- lapply(data$symptoms_clean[grep('diarrhea|diarrhoea|diarrheoa',data$symptoms)], function(x) paste("Diarrhea",x,sep=","))
data$symptoms_clean[grep('asymptomatic|afebrile|no serious symptoms|none|oligosymptomatic|no symptoms|no clinical symptoms',data$symptoms)] <- lapply(data$symptoms_clean[grep('asymptomatic|afebrile|no serious symptoms|none|oligosymptomatic|no symptoms|no clinical symptoms',data$symptoms)], function(x) paste("Asymptomatic",x,sep=","))
data$symptoms_clean[grep('kidney|renal',data$symptoms)] <- lapply(data$symptoms_clean[grep('kidney|renal',data$symptoms)], function(x) paste("Kidney Injury",x,sep=","))
data$symptoms_clean[grep('heart|cardiogenic|cardiac|myocardial|cardiopulmonary',data$symptoms)] <- lapply(data$symptoms_clean[grep('heart|cardiogenic|cardiac|myocardial|cardiopulmonary',data$symptoms)], function(x) paste("Heart Failure",x,sep=","))
data$symptoms_clean[grep('septic',data$symptoms)] <- lapply(data$symptoms_clean[grep('septic',data$symptoms)], function(x) paste("Septic Shock",x,sep=","))
data$symptoms_clean[grep('^mild$',data$symptoms)] <- "Missing"
data$symptoms_clean[grep('^weakness$',data$symptoms)] <- "Missing"
data$symptoms_clean[grep('^severe$',data$symptoms)] <- "Missing"
data$symptoms_clean[grep('^pneumonia$',data$symptoms)] <- "Missing"
data$symptoms_clean[grep('^pulmonary$',data$symptoms)] <- "Missing"
data$symptoms_clean[grep('^fatigue$',data$symptoms)] <- "Missing"
data$symptoms_clean[grep('^discomfort$',data$symptoms)] <- "Missing"

data$symptoms_clean[grep('mild:moderate|mild to moderate|mild symptoms|covid|pnuemonia|pneumonitis|lesions|physical|yes|flulike|somnolence|hcovbrazilspbr|myelofibrosis',data$symptoms)] <- "Missing"

## Cleaned symptoms
u<-unique(data$symptoms_clean)

## Classified Unique Symptoms
us <- as.character(data$symptoms_clean)
us_vec <- unlist(strsplit(us,','))
unique(us_vec)

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



