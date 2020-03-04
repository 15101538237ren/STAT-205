FEVdata <- read.table(file="FullFEVdata.txt",header=T, sep="") 
attach(FEVdata)
pairs(~Age+Hgt+Male+Smoke+FEV,labels=c("Age","Height","Male","Smoke","FEV"),data=FEVdata)