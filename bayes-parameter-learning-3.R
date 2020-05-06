library(dplyr)

data <- read.csv('D:/Ziwei Su/SchoolWorks/Purdue/20 Spring/546 pj/data/cleaned_data.csv')


## learn bayesian network structure
library(bnlearn)
node<-colnames(data)
node<-node[2:length(node)]
elements.2.remove<-"Missing"
node<-node[!(node %in% elements.2.remove)]
data<-data[,node]
n<-length(node)
e<-empty.graph(node)
# create arc set
arc.set<-matrix(NA,nrow=19,ncol=2,byrow = TRUE,dimnames = list(NULL, c("from", "to")))
arc.set[,1]<-c(rep("Gender",3),rep("Age",3),rep("Fever",3),rep("Cough",3),
               "Body.Pain","Diarrhea","Anorexia",
               "Rhinorrhea","Chills",rep("Dyspnea",2))
des.gender<-c("Fever","Rhinorrhea","Outcome")
des.age<-c("Dyspnea","Asymptomatic","Outcome")
des.fever<-c("Dyspnea","Septic.Shock","Outcome")
des.cough<-c("Septic.Shock","Heart.Failure","Outcome")
des.body.pain<-c("Fever")
des.diarrhea<-c("Rhinorrhea")
des.anorexia<-c("Fever")
des.rhinorrhea<-c("Body.Pain")
des.chills<-c("Body.Pain")
des.dyspnea<-c("Kidney.Injury","Outcome")
arc.set[,2]<-c(des.gender,des.age,des.fever,des.cough,des.body.pain,des.diarrhea,des.anorexia,des.rhinorrhea,des.chills,des.dyspnea)
arcs(e) <- arc.set
# fitting
for(i in 1:n){
  data[,i]<-as.factor(data[,i])
}
# structure<-iamb(data)
# print(structure)
result<-bn.fit(e,data,method="bayes")
print(result)