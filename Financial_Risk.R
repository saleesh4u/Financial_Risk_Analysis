# Installing and loading ibraries

library(readxl)
library(caTools)   
library(car)
library(caret)
library(ROCR)
library(corrplot)
library(car)

#Data upload and cleaning

setwd("/Users/saleesh/Desktop/R Programming")
IndCredData=read_xlsx("Creditdata.xlsx")
View(IndCredData)
summary(IndCredData)
str(IndCredData)
dim(IndCredData)

IndCredData$`Creditors turnover`=as.numeric(IndCredData$`Creditors turnover`)
IndCredData$`Debtors turnover`=as.numeric(IndCredData$`Debtors turnover`)
IndCredData$`Finished goods turnover`=as.numeric(IndCredData$`Finished goods turnover`)
IndCredData$`WIP turnover`=as.numeric(IndCredData$`WIP turnover`)
IndCredData$`Raw material turnover`=as.numeric(IndCredData$`Raw material turnover`)
IndCredData$`Shares outstanding`=as.numeric(IndCredData$`Shares outstanding`)
IndCredData$`Equity face value`=as.numeric(IndCredData$`Equity face value`)
IndCredData$`PE on BSE`=as.numeric(IndCredData$`PE on BSE`)

IndCredData=IndCredData[ ,c(-1,-18,-19,-22,-25,-32,-34,-52)] #Removing variables that are insignificant or having too many NA values.

#Selecting Variables from Buckets 
#Profitability - Cumulative retained profit, EPS
#Leverage - Total Liabilities,Current Ratio
#Liquidity - Debt to Equity Ratio, Current Liabilities and Provisions
#Size - Equity Face Value, Capital Employed

#Outlier Treatment

boxplot(`Cumulative retained profits`)
boxplot(EPS)
boxplot(`Total liabilities`)
boxplot(`Current ratio (times)`)
boxplot(`Debt to equity ratio (times)`)
boxplot(`Current liabilities & provisions`)
boxplot(`Equity face value`)
boxplot(`Capital employed`)

SelCreData=IndCredData[,c(1,20,22,23,31,32,41,42,44)]

#Checking for missing values and treading them
sum(is.na(SelCreData))
colSums(is.na(SelCreData))

NoNAData=na.omit(SelCreData)

#Univariate and Bivariate analysis
plot(`Cumulative retained profits`)
summary(`Cumulative retained profits`)
plot(EPS)
summary(EPS)
plot(`Total liabilities`)
summary(`Total liabilities`)
plot(`Current ratio (times)`)
summary(`Current ratio (times)`)
plot(`Debt to equity ratio (times)`)
summary(`Debt to equity ratio (times)`)
plot(`Current liabilities & provisions`)
summary(`Current liabilities & provisions`)
plot(`Equity face value`)
summary(`Equity face value`)
plot(`Capital employed`)
summary(`Capital employed`)

plot(`Cumulative retained profits`,`Total liabilities`)
plot(`Cumulative retained profits`,`Debt to equity ratio (times)`)
plot(`Cumulative retained profits`,`Capital employed`)
plot(`Current ratio (times)`,EPS)
plot(`Current ratio (times)`,`Current liabilities & provisions`)
plot(`Current ratio (times)`,`Capital employed`)
plot(`Current liabilities & provisions`,EPS)
plot(`Current liabilities & provisions`,`Total liabilities`)
plot(`Current liabilities & provisions`,`Capital employed`)

#Creating new binary variable "Default"
Default=ifelse(NoNAData$`Networth Next Year`>0, 0,1)
Default=as.factor(Default)
summary(Default)
prop.table(table(Default))

FinalData=cbind(NoNAData[ ,-1],Default)
attach(FinalData)

#Checking for Multicollinearity

cor(FinalData[ ,-9])
correlation=corrplot(cor(FinalData[ ,-9]),method = "circle",type = "upper")


model1=glm(Default~`Current liabilities & provisions`+`Cumulative retained profits`+
            `Capital employed`+`Current ratio (times)`+`Debt to equity ratio (times)`+`Equity face value`+EPS+
            `Total liabilities`,data=FinalData,family=binomial("logit"))
summary(model1)

vif(model1)

#Rebulding logistic regression model after removing high vif variables.

model2=glm(Default~`Cumulative retained profits`+`Current ratio (times)`+`Debt to equity ratio (times)`+
             `Equity face value`+EPS,data=FinalData,family=binomial("logit"))
summary(model2)
vif(model2)

#Preparing Validation data

ValiData=read_xlsx("Validationdata.xlsx")
ValiData$`Equity face value`=as.numeric(ValiData$`Equity face value`)


# Checking Default Ratio between Training Data and Validation Data
prop.table(table(Default))
prop.table(table(ValiData$`Default - 1`))

#Model Accuracy
predTest=predict(model2,newdata = ValiData,type = "response")

#Creation of confusion matrix to assess model performance measures
tab.LR=table(ValiData$`Default - 1`,predTest > 0.5)
tab.LR
sum(diag(tab.LR))/sum(tab.LR)




