rm(list=ls())
library(arules)
setwd("C:/Users/kumru/OneDrive/Belgeler/PhD/stats780/week 1")
source("std_lift.R")
x = read.csv("Arrests1.csv")
str(x)

x$year <- factor(x$year)
x$checks <- factor(x$checks)
x$age <- NULL
x$X <- NULL
str(x)

params <- list(support=0.2, confidence=0.8, minlen=2, maxlen=8)
fit1 <- apriori(x, parameter=params)
quality(fit1) <- std_lift(fit1,x)
inspect(sort(fit1, by="slift"))

str(x[x$colour=="White",])

# interested in release
params <- list(support=0.2, confidence=0.8, minlen=2, maxlen=8)
app <- list(default="lhs", rhs=c("released=No","released=Yes"))
fit2 <- apriori(x,parameter=params,control=list(verbose=FALSE),appearance=app)
quality(fit2)<-std_lift(fit2,x)
inspect(sort(fit2, by = "slift"))

# released no doesn't work too low support
params <- list(support=0.0008, confidence=0.8, minlen=2, maxlen=8)
app<-list(default="none",rhs=c("released=No"),lhs=c("colour=White","colour=Black","sex=Male","sex=Female","citizen=Yes","citizen=No","employed=Yes","employed=No","checks=0","checks=1","checks=2","checks=3","checks=4","checks=5","checks=6"))
fit4<-apriori(x,parameter=params,control=list(verbose=FALSE),appearance=app)
quality(fit4)<-std_lift(fit4,x)
inspect(sort(fit4, by = "slift"))

# released no with slightly low support
params <- list(support=0.0008, confidence=0.8, minlen=2, maxlen=8)
app<-list(default="lhs",rhs=c("released=No"))
fit4<-apriori(x,parameter=params,control=list(verbose=FALSE),appearance=app)
quality(fit4)<-std_lift(fit4,x)
inspect(sort(fit4, by = "slift"))

# colour to arrests relationship
params <- list(support=0.005, confidence=0.8, minlen=2, maxlen=8)
app<-list(default="lhs",rhs=c("colour=Black"))
fit5<-apriori(x,parameter=params,control=list(verbose=FALSE),appearance=app)
quality(fit5)<-std_lift(fit5,x)
inspect(sort(fit5, by = "slift"))
