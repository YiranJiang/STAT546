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
arc.set<-matrix(NA,nrow=65,ncol=2,byrow = TRUE,dimnames = list(NULL, c("from", "to")))
arc.set[,1]<-c(rep("Gender",13),rep("Age",13),rep("Fever",5),rep("Cough",5),
               rep("Body.Pain",5),rep("Diarrhea",5),rep("Anorexia",5),
               rep("Rhinorrhea",5),rep("Chills",5),"Dyspnea","Kidney.Injury",
               "Heart.Failure","Septic.Shock")
arc.set[,2]<-c(rep(node[3:n],2),rep(c(node[5],node[12:15]),7),rep("Outcome",4))
arcs(e) <- arc.set
# fitting
for(i in 1:n){
  data[,i]<-as.factor(data[,i])
}
structure<-iamb(data)
print(structure)
# To see the adjacency matrix, use amat()
result<-bn.fit(e,data,method="bayes")
print(result)
