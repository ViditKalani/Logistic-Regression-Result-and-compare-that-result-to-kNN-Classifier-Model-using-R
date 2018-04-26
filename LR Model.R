library(tidyverse)
library(caret)
library(mlbench)
library(glmnet)
library(glmnetUtils)
#install.packages('e1071', dependencies=TRUE)

data(GermanCredit)
GermanCredit <- as.tibble(GermanCredit)
head(GermanCredit)
select(GermanCredit,
       Duration,
       Amount,
       InstallmentRatePercentage,
       Age,
       NumberExistingCredits,
       NumberPeopleMaintenance,
       Telephone,
       ForeignWorker,
       Class,
       CheckingAccountStatus.lt.0,
       CheckingAccountStatus.0.to.200,
       CheckingAccountStatus.gt.200,
       CheckingAccountStatus.none,
       CreditHistory.NoCredit.AllPaid,
       CreditHistory.ThisBank.AllPaid,
       CreditHistory.PaidDuly,
       CreditHistory.Delay,
       CreditHistory.Critical,
       SavingsAccountBonds.lt.100,
       SavingsAccountBonds.100.to.500,
       SavingsAccountBonds.500.to.1000,
       SavingsAccountBonds.gt.1000,
       SavingsAccountBonds.Unknown,
       EmploymentDuration.lt.1,
       EmploymentDuration.1.to.4,
       EmploymentDuration.4.to.7,
       EmploymentDuration.gt.7,
       EmploymentDuration.Unemployed,
       Personal.Male.Divorced.Seperated,
       Personal.Female.NotSingle,
       Personal.Male.Single,
       Personal.Male.Married.Widowed,
       Personal.Female.Single,
       Property.RealEstate,
       Property.Insurance,
       Property.CarOther,
       Property.Unknown,
       Housing.Rent,
       Housing.Own,
       Housing.ForFree
)
GermanCredit <- select(GermanCredit,
                       Duration,
                       Amount,
                       InstallmentRatePercentage,
                       Age,
                       NumberExistingCredits,
                       NumberPeopleMaintenance,
                       Telephone,
                       ForeignWorker,
                       Class,
                       CheckingAccountStatus.lt.0,
                       CheckingAccountStatus.0.to.200,
                       CheckingAccountStatus.gt.200,
                       CheckingAccountStatus.none,
                       CreditHistory.NoCredit.AllPaid,
                       CreditHistory.ThisBank.AllPaid,
                       CreditHistory.PaidDuly,
                       CreditHistory.Delay,
                       CreditHistory.Critical,
                       SavingsAccountBonds.lt.100,
                       SavingsAccountBonds.100.to.500,
                       SavingsAccountBonds.500.to.1000,
                       SavingsAccountBonds.gt.1000,
                       SavingsAccountBonds.Unknown,
                       EmploymentDuration.lt.1,
                       EmploymentDuration.1.to.4,
                       EmploymentDuration.4.to.7,
                       EmploymentDuration.gt.7,
                       EmploymentDuration.Unemployed,
                       Personal.Male.Divorced.Seperated,
                       Personal.Female.NotSingle,
                       Personal.Male.Single,
                       Personal.Male.Married.Widowed,
                       Personal.Female.Single,
                       Property.RealEstate,
                       Property.Insurance,
                       Property.CarOther,
                       Property.Unknown,
                       Housing.Rent,
                       Housing.Own,
                       Housing.ForFree
)
head(GermanCredit)


set.seed(1)
trainIndex <- createDataPartition(GermanCredit$Class,
                                  p = 0.8, 
                                  list = FALSE, 
                                  times = 1)

GermanCredit[trainIndex, ]
germanTrain <- GermanCredit[trainIndex, ]

GermanCredit[-trainIndex, ]
germanTest <- GermanCredit[-trainIndex, ]

preProcess(germanTrain, method = c("center", "scale"))
scaler <- preProcess(germanTrain, 
                     method = c("center", "scale"))

predict(scaler, germanTrain)
germanTrain <- predict(scaler, germanTrain)
predict(scaler, germanTest)
germanTest <- predict(scaler, germanTest)
head(germanTrain)

# Round: 1

lr <- glmnet(Class ~ .,
             data = germanTrain,
             family = "binomial",
             na.action = na.pass)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)
head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
               germanTest$Class)

lr <- glmnet(Class ~ Duration + Amount,
       data = germanTrain,
       family = "binomial",
       na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               InstallmentRatePercentage,
               data = germanTrain,
               family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + Age,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)


lr <- glmnet(Class ~ Duration + NumberExistingCredits,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + NumberPeopleMaintenance,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + Telephone,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)
head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + ForeignWorker,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)
head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)


lr <- glmnet(Class ~ Duration + 
               CheckingAccountStatus.lt.0 + 
               CheckingAccountStatus.0.to.200 + 
               CheckingAccountStatus.gt.200 + 
               CheckingAccountStatus.none, 
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               CreditHistory.NoCredit.AllPaid +
               CreditHistory.ThisBank.AllPaid +
               CreditHistory.PaidDuly + 
               CreditHistory.Delay + 
               CreditHistory.Critical,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)


lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown, 
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration +
               EmploymentDuration.lt.1 + 
               EmploymentDuration.1.to.4 + 
               EmploymentDuration.4.to.7 + 
               EmploymentDuration.gt.7 + 
               EmploymentDuration.Unemployed,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               Personal.Male.Divorced.Seperated + 
               Personal.Female.NotSingle + 
               Personal.Male.Single + 
               Personal.Male.Married.Widowed + 
               Personal.Female.Single,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               Property.RealEstate + 
               Property.Insurance +
               Property.CarOther + 
               Property.Unknown, 
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               Housing.Rent +
               Housing.Own +
               Housing.ForFree, 
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               Housing.Rent +
               Housing.Own +
               Housing.ForFree, 
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

# Round: 2

lr <- glmnet(Class ~ Duration + SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown + 
               Amount,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               InstallmentRatePercentage,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown + 
               Age,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)


lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown + 
               NumberExistingCredits,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown + 
               NumberPeopleMaintenance,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown + 
               Telephone,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)
head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown + 
               ForeignWorker,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)
head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)


lr <- glmnet(Class ~ Duration + SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               CheckingAccountStatus.lt.0 + 
               CheckingAccountStatus.0.to.200 + 
               CheckingAccountStatus.gt.200 + 
               CheckingAccountStatus.none, 
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               CreditHistory.NoCredit.AllPaid +
               CreditHistory.ThisBank.AllPaid +
               CreditHistory.PaidDuly + 
               CreditHistory.Delay + 
               CreditHistory.Critical,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               EmploymentDuration.lt.1 + 
               EmploymentDuration.1.to.4 + 
               EmploymentDuration.4.to.7 + 
               EmploymentDuration.gt.7 + 
               EmploymentDuration.Unemployed,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               Personal.Male.Divorced.Seperated + 
               Personal.Female.NotSingle + 
               Personal.Male.Single + 
               Personal.Male.Married.Widowed + 
               Personal.Female.Single,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               Property.RealEstate + 
               Property.Insurance +
               Property.CarOther + 
               Property.Unknown, 
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               Housing.Rent +
               Housing.Own +
               Housing.ForFree, 
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

# Round: 3

lr <- glmnet(Class ~ Duration + SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown + 
               InstallmentRatePercentage +
               Amount,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown + 
               InstallmentRatePercentage +
               Age,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)


lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               InstallmentRatePercentage +
               NumberExistingCredits,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown + 
               InstallmentRatePercentage +
               NumberPeopleMaintenance,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown + 
               InstallmentRatePercentage +
               Telephone,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)
head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               InstallmentRatePercentage +
               ForeignWorker,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)
head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)


lr <- glmnet(Class ~ Duration + SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               InstallmentRatePercentage +
               CheckingAccountStatus.lt.0 + 
               CheckingAccountStatus.0.to.200 + 
               CheckingAccountStatus.gt.200 + 
               CheckingAccountStatus.none, 
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               InstallmentRatePercentage +
               CreditHistory.NoCredit.AllPaid +
               CreditHistory.ThisBank.AllPaid +
               CreditHistory.PaidDuly + 
               CreditHistory.Delay + 
               CreditHistory.Critical,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               InstallmentRatePercentage +
               EmploymentDuration.lt.1 + 
               EmploymentDuration.1.to.4 + 
               EmploymentDuration.4.to.7 + 
               EmploymentDuration.gt.7 + 
               EmploymentDuration.Unemployed,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               InstallmentRatePercentage +
               Personal.Male.Divorced.Seperated + 
               Personal.Female.NotSingle + 
               Personal.Male.Single + 
               Personal.Male.Married.Widowed + 
               Personal.Female.Single,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               InstallmentRatePercentage +
               Property.RealEstate + 
               Property.Insurance +
               Property.CarOther + 
               Property.Unknown, 
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               InstallmentRatePercentage +
               Housing.Rent +
               Housing.Own +
               Housing.ForFree, 
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)


# Round: 4

lr <- glmnet(Class ~ Duration + SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown + 
               InstallmentRatePercentage +
               Amount + 
               Age,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)


lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               InstallmentRatePercentage +
               Amount + 
               NumberExistingCredits,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown + 
               InstallmentRatePercentage +
               Amount +
               NumberPeopleMaintenance,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown + 
               InstallmentRatePercentage +
               Amount +
               Telephone,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)
head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               InstallmentRatePercentage +
               Amount +
               ForeignWorker,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)
head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)


lr <- glmnet(Class ~ Duration + SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               InstallmentRatePercentage +
               Amount +
               CheckingAccountStatus.lt.0 + 
               CheckingAccountStatus.0.to.200 + 
               CheckingAccountStatus.gt.200 + 
               CheckingAccountStatus.none, 
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               InstallmentRatePercentage +
               Amount +
               CreditHistory.NoCredit.AllPaid +
               CreditHistory.ThisBank.AllPaid +
               CreditHistory.PaidDuly + 
               CreditHistory.Delay + 
               CreditHistory.Critical,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               InstallmentRatePercentage +
               Amount +
               EmploymentDuration.lt.1 + 
               EmploymentDuration.1.to.4 + 
               EmploymentDuration.4.to.7 + 
               EmploymentDuration.gt.7 + 
               EmploymentDuration.Unemployed,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               InstallmentRatePercentage +
               Amount +
               Personal.Male.Divorced.Seperated + 
               Personal.Female.NotSingle + 
               Personal.Male.Single + 
               Personal.Male.Married.Widowed + 
               Personal.Female.Single,
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               InstallmentRatePercentage +
               Amount +
               Property.RealEstate + 
               Property.Insurance +
               Property.CarOther + 
               Property.Unknown, 
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)

lr <- glmnet(Class ~ Duration + 
               SavingsAccountBonds.lt.100 + 
               SavingsAccountBonds.100.to.500 + 
               SavingsAccountBonds.500.to.1000 + 
               SavingsAccountBonds.gt.1000 + 
               SavingsAccountBonds.Unknown +
               InstallmentRatePercentage +
               Amount +
               Housing.Rent +
               Housing.Own +
               Housing.ForFree, 
             data = germanTrain,
             family = "binomial",
             na.action = na.omit)
predictions = predict(lr,
                      germanTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 = predict(lr,
                       germanTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                germanTest$Class)
