library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)
library(randomForest)
library(ISLR)
library(rpart)
library(rpart.plot)
library(e1071)
library(pROC)
#install.packages("moments")
library(moments)
#install.packages("corrplot")
library(corrplot)
library(ggplot2)
#install.packages("ggcorrplot")
library(ggcorrplot)
library(e1071)
#Load the dataset
ibm <- read.csv("50000 HRA Records.csv")
str(ibm)
head(ibm)
ibm$Attrition<- NULL
#Find and impute missing values 
# apply function anyNA() on all columns of  dataset
col1<- mapply(anyNA,ibm)
col1
#Change the data types

ibm$Attrition[ibm$Attrition=="Yes"]<-1
ibm$Attrition[ibm$Attrition=="No"]<-0
ibm$Attrition=as.numeric(ibm$Attrition)

#change all “character” variables into “Factor”
ibm[,c(3,5,7,8,12,16,18,23)]=lapply(ibm[,c(3,5,7,8,12,16,18,23)],as.factor)
str(ibm)
ibm$Over18[ibm$Over18=="Y"]<-1
ibm$Over18=as.numeric(ibm$Over18)
#Splitting the dataset into “training” and “testing”
set.seed(1000)
ranuni=sample(x=c("Training","Testing"),size=nrow(ibm),replace=T,prob=c(0.7,0.3))
TrainingData=ibm[ranuni=="Training",]
TestingData=ibm[ranuni=="Testing",]
nrow(TrainingData)
nrow(TestingData)

#Building up the model
#Identify the independent variables
#Incorporate the dependent variable “Attrition” in the model
#Transform the data type of model from “character” to “formula”
#Incorporate TRAINING data into the formula and build the model
independentvariables=colnames(ibm[,2:35])
independentvariables
Model=paste(independentvariables,collapse="+")
Model
Model_1=paste("Attrition~",Model)
Model_1
class(Model_1)
formula=as.formula(Model_1)
formula
#incorporate “Training Data” into the formula using the “glm” function and build up a logistic regression model.

Trainingmodel1=glm(formula=formula,data=TrainingData,family="binomial")
Trainingmodel1=step(object = Trainingmodel1,direction = "both")
summary(Trainingmodel1)



#GG plot
library(ggplot2)
library(ggalluvial)
library(GGally)
ggpairs(data = ibm[5],
        title = "Histogram",
        upper = list(continuous = wrap("cor", size = 5)), 
        lower = list(continuous = "smooth")
)

# Heatmap
ggp <- ggplot(ibm, aes(Attrition,Department)) + # Create heatmap with ggplot2
  geom_tile(aes(fill = Gender))
ggp

#scatter
ggplot(data=ibm,aes(x=BusinessTravel,y=Department,color=Gender))+geom_point(alpha=2)

#Box plot
#box plot with blue color
ggplot(data = 
         ibm,aes(y=JobRole))+geom_boxplot(color = "blue")






