library(tidyverse)
library(caret)
# install.packages('e1071', dependencies=TRUE)
data()

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

ggplot(data = GermanCredit)+
  geom_boxplot(mapping = aes(x = Class, y = Duration))

ggplot(data = GermanCredit)+
  geom_boxplot(mapping = aes(x = Class, y = Amount))

ggplot(data = GermanCredit)+
  geom_boxplot(mapping = aes(x = Class, y = InstallmentRatePercentage))

ggplot(data = GermanCredit)+
  geom_boxplot(mapping = aes(x = Class, y = Age))

ggplot(data = GermanCredit) +
  geom_boxplot(mapping = aes(x = Class, y = NumberExistingCredits))

ggplot(data = GermanCredit) +
  geom_boxplot(mapping = aes(x = Class, y = NumberPeopleMaintenance))

ggplot(data = GermanCredit) +
  geom_boxplot(mapping = aes(x = Class, y = Telephone))

ggplot(data = GermanCredit) +
  geom_boxplot(mapping = aes(x = Class, y = ForeignWorker))

GermanCredit <- mutate(GermanCredit,
                       CheckingAccountStatus =
                         as.factor(ifelse(GermanCredit$CheckingAccountStatus.lt.0 > 0, "lt.0",
                                          ifelse(GermanCredit$CheckingAccountStatus.0.to.200 > 0, "0.to.200",
                                          ifelse(GermanCredit$CheckingAccountStatus.0.to.200 > 0, "gt.200",
                                          ifelse(GermanCredit$CheckingAccountStatus.none > 0, "none","none"))))))
ggplot(data = GermanCredit, mapping = aes(x = CheckingAccountStatus, y = Class)) + geom_jitter()

GermanCredit <- mutate(GermanCredit,
                       CreditHistory =
                         as.factor(ifelse(GermanCredit$CreditHistory.Critical > 0, "Critical",
                                          ifelse(GermanCredit$CreditHistory.Delay > 0, "Delay",
                                          ifelse(GermanCredit$CreditHistory.NoCredit.AllPaid > 0, "NoCredit_AllPaid",
                                          ifelse(GermanCredit$CreditHistory.PaidDuly > 0, "PaidDuly",
                                          ifelse(GermanCredit$CreditHistory.ThisBank.AllPaid > 0, "ThisBank_AllPaid", "none")))))))
ggplot(data = GermanCredit, mapping = aes(x = Class, y = CreditHistory)) + geom_jitter()


GermanCredit <- mutate(GermanCredit,
                       SavingsAccountBonds =
                         as.factor(ifelse(GermanCredit$SavingsAccountBonds.lt.100 > 0, "lt.100",
                                  ifelse(GermanCredit$SavingsAccountBonds.100.to.500 > 0, "100.to.500",
                                  ifelse(GermanCredit$SavingsAccountBonds.500.to.1000 > 0, "500.to.1000",
                                  ifelse(GermanCredit$SavingsAccountBonds.gt.1000 > 0, "gt.1000",
                                  ifelse(GermanCredit$SavingsAccountBonds.Unknown > 0, "Unknown", "none")))))))
ggplot(data = GermanCredit, mapping = aes(y = SavingsAccountBonds, x = Class)) + geom_jitter()


GermanCredit <- mutate(GermanCredit,
                       EmploymentDuration =
                         as.factor(ifelse(GermanCredit$EmploymentDuration.lt.1 > 0, "lt.1",
                                         ifelse(GermanCredit$EmploymentDuration.1.to.4 > 0, "1.to.4",
                                         ifelse(GermanCredit$EmploymentDuration.4.to.7 > 0, "4.to.7",
                                         ifelse(GermanCredit$EmploymentDuration.gt.7 > 0, "gt.7",
                                         ifelse(GermanCredit$EmploymentDuration.Unemployed > 0, "Unemployed", "none")))))))
ggplot(data = GermanCredit, mapping = aes(x = Class, y = EmploymentDuration)) + geom_jitter()


GermanCredit <- mutate(GermanCredit,
                       Personal =
                         as.factor(ifelse(GermanCredit$Personal.Male.Divorced.Seperated > 0, "Male.Divorced.Seperated",
                                         ifelse(GermanCredit$Personal.Female.NotSingle > 0, "Female.NotSingle",
                                         ifelse(GermanCredit$Personal.Male.Single > 0, "Male.Single",
                                         ifelse(GermanCredit$Personal.Male.Married.Widowed > 0, "Male.Married.Widowed",
                                         ifelse(GermanCredit$Personal.Female.Single > 0, "Female.Single", "none")))))))
ggplot(data = GermanCredit, mapping = aes(x = Class, y = Personal)) + geom_jitter()

GermanCredit <- mutate(GermanCredit,
                       Housing =
                         as.factor(ifelse(GermanCredit$Housing.Rent > 0, "Rent",
                                          ifelse(GermanCredit$Housing.Own > 0, "Own",
                                          ifelse(GermanCredit$Housing.ForFree > 0, "ForFree", "none")))))
ggplot(data = GermanCredit, mapping = aes(x = Class, y = Housing)) + geom_jitter()

?set.seed
set.seed(221)
trainIndex <- createDataPartition(GermanCredit$Class, p = 0.8, list = FALSE, times = 1)

GermanCredit[trainIndex, ]
germanTrain <- GermanCredit[trainIndex, ]

GermanCredit[-trainIndex, ]
germanTest <- GermanCredit[-trainIndex, ]

preProcess(germanTrain, method = c("center", "scale"))
scaler <- preProcess(germanTrain, method = c("center", "scale"))

predict(scaler, germanTrain)
germanTrain <- predict(scaler, germanTrain)
predict(scaler, germanTest)
germanTest <- predict(scaler, germanTest)
head(germanTrain)


train(Class ~ Duration, data = germanTrain, method = "knn")
knn_model_Duration <- train(Class ~ Duration, data = germanTrain, method = "knn")
predict(knn_model_Duration, germanTest)
germanTestPredictions_Duration <- predict(knn_model_Duration, germanTest)
confusionMatrix(germanTestPredictions_Duration, germanTest$Class)


train(Class ~ Amount, data = germanTrain, method = "knn")
knn_model_Amount <- train(Class ~ Amount, data = germanTrain, method = "knn")
predict(knn_model_Amount, germanTest)
germanTestPredictions_Amount <- predict(knn_model_Amount, germanTest)
confusionMatrix(germanTestPredictions_Amount, germanTest$Class)


train(Class ~ InstallmentRatePercentage, data = germanTrain, method = "knn")
knn_model_InstallmentRatePercentage <- train(Class ~ InstallmentRatePercentage, data = germanTrain, method = "knn")
predict(knn_model_InstallmentRatePercentage, germanTest)
germanTestPredictions_InstallmentRatePercentage <-predict(knn_model_InstallmentRatePercentage, germanTest)
confusionMatrix(germanTestPredictions_InstallmentRatePercentage, germanTest$Class)


train(Class ~ Age, data = germanTrain, method = "knn")
knn_model_Age <- train(Class ~ Age, data = germanTrain, method = "knn")
predict(knn_model_Age, germanTest)
germanTestPredictions_Age <- predict(knn_model_Age, germanTest)
confusionMatrix(germanTestPredictions_Age, germanTest$Class)


train(Class ~ NumberExistingCredits, data = germanTrain, method = "knn")
knn_model_NumberExistingCredits <- train(Class ~ NumberExistingCredits, data = germanTrain, method = "knn")
predict(knn_model_NumberExistingCredits, germanTest)
germanTestPredictions_NumberExistingCredits <- predict(knn_model_NumberExistingCredits, germanTest)
confusionMatrix(germanTestPredictions_NumberExistingCredits, germanTest$Class)


train(Class ~ NumberPeopleMaintenance, data = germanTrain, method = "knn")
knn_model_NumberPeopleMaintenance <- train(Class ~ NumberPeopleMaintenance, data = germanTrain, method = "knn")
predict(knn_model_NumberPeopleMaintenance, germanTest)
germanTestPredictions_NumberPeopleMaintenance <- predict(knn_model_NumberPeopleMaintenance, germanTest)
confusionMatrix(germanTestPredictions_NumberPeopleMaintenance, germanTest$Class)


train(Class ~ Telephone, data = germanTrain, method = "knn")
knn_model_Telephone <- train(Class ~ Telephone, data = germanTrain, method = "knn")
predict(knn_model_Telephone, germanTest)
germanTestPredictions_Telephone <- predict(knn_model_Telephone, germanTest)
confusionMatrix(germanTestPredictions_Telephone, germanTest$Class)


train(Class ~ ForeignWorker, data = germanTrain, method = "knn")
knn_model_ForeignWorker <- train(Class ~ ForeignWorker, data = germanTrain, method = "knn")
predict(knn_model_ForeignWorker, germanTest)
germanTestPredictions_ForeignWorker <- predict(knn_model_ForeignWorker, germanTest)
confusionMatrix(germanTestPredictions_ForeignWorker, germanTest$Class)


train(Class ~ CheckingAccountStatus.lt.0 + 
              CheckingAccountStatus.0.to.200 + 
              CheckingAccountStatus.gt.200 + 
              CheckingAccountStatus.none, 
              data = germanTrain, method = "knn")
knn_model_CheckingAccountStatus <- train(Class ~ CheckingAccountStatus.lt.0 + 
                                                 CheckingAccountStatus.0.to.200 + 
                                                 CheckingAccountStatus.gt.200 + 
                                                 CheckingAccountStatus.none, 
                                                 data = germanTrain, method = "knn")
predict(knn_model_CheckingAccountStatus, germanTest)
germanTestPredictions_CheckingAccountStatus <- predict(knn_model_CheckingAccountStatus, germanTest)
confusionMatrix(germanTestPredictions_CheckingAccountStatus, germanTest$Class)


train(Class ~ CreditHistory.NoCredit.AllPaid + 
              CreditHistory.ThisBank.AllPaid + 
              CreditHistory.PaidDuly + 
              CreditHistory.Delay + 
              CreditHistory.Critical, 
              data = germanTrain, method = "knn")
knn_model_CreditHistory <- train(Class ~ CreditHistory.NoCredit.AllPaid +
                                         CreditHistory.ThisBank.AllPaid +
                                         CreditHistory.PaidDuly + 
                                         CreditHistory.Delay + 
                                         CreditHistory.Critical, 
                                         data = germanTrain, method = "knn")
predict(knn_model_CreditHistory, germanTest)
germanTestPredictions_CreditHistory <- predict(knn_model_CreditHistory, germanTest)
confusionMatrix(germanTestPredictions_CreditHistory, germanTest$Class)


train(Class ~ SavingsAccountBonds.lt.100 + 
              SavingsAccountBonds.100.to.500 + 
              SavingsAccountBonds.500.to.1000 + 
              SavingsAccountBonds.gt.1000 + 
              SavingsAccountBonds.Unknown, 
              data = germanTrain, method = "knn")
knn_model_SavingsAccountBonds <- train(Class ~ SavingsAccountBonds.lt.100 + 
                                               SavingsAccountBonds.100.to.500 + 
                                               SavingsAccountBonds.500.to.1000 + 
                                               SavingsAccountBonds.gt.1000 + 
                                               SavingsAccountBonds.Unknown, 
                                               data = germanTrain, method = "knn")
predict(knn_model_SavingsAccountBonds, germanTest)
germanTestPredictions_SavingsAccountBonds <- predict(knn_model_SavingsAccountBonds, germanTest)
confusionMatrix(germanTestPredictions_SavingsAccountBonds, germanTest$Class)


train(Class ~ EmploymentDuration.lt.1 + 
              EmploymentDuration.1.to.4 + 
              EmploymentDuration.4.to.7 + 
              EmploymentDuration.gt.7 + 
              EmploymentDuration.Unemployed, 
              data = germanTrain, method = "knn")
knn_model_EmploymentDuration <- train(Class ~ EmploymentDuration.lt.1 +
                                              EmploymentDuration.1.to.4 + 
                                              EmploymentDuration.4.to.7 + 
                                              EmploymentDuration.gt.7 +
                                              EmploymentDuration.Unemployed, 
                                              data = germanTrain, method = "knn")
predict(knn_model_EmploymentDuration, germanTest)
germanTestPredictions_EmploymentDuration <- predict(knn_model_EmploymentDuration, germanTest)
confusionMatrix(germanTestPredictions_EmploymentDuration, germanTest$Class)


train(Class ~ Personal.Male.Divorced.Seperated + 
              Personal.Female.NotSingle + 
              Personal.Male.Single + 
              Personal.Male.Married.Widowed + 
              Personal.Female.Single,
              data = germanTrain, method = "knn")
knn_model_Personal <- train(Class ~ Personal.Male.Divorced.Seperated + 
                                    Personal.Female.NotSingle + 
                                    Personal.Male.Single + 
                                    Personal.Male.Married.Widowed +
                                    Personal.Female.Single, 
                                    data = germanTrain, method = "knn")
predict(knn_model_Personal, germanTest)
germanTestPredictions_Personal <- predict(knn_model_Personal, germanTest)
confusionMatrix(germanTestPredictions_Personal, germanTest$Class)


train(Class ~  Property.RealEstate + 
               Property.Insurance + 
               Property.CarOther + 
               Property.Unknown, 
               data = germanTrain, method = "knn")
knn_model_Property <- train(Class ~ Property.RealEstate + 
                                    Property.Insurance + 
                                    Property.CarOther +
                                    Property.Unknown,
                                    data = germanTrain, method = "knn")
predict(knn_model_Property, germanTest)
germanTestPredictions_Property <- predict(knn_model_Property, germanTest)
confusionMatrix(germanTestPredictions_Property, germanTest$Class)


train(Class ~ Housing.Rent + 
              Housing.Own + 
              Housing.ForFree, 
              data = germanTrain, method = "knn")
knn_model_Housing <- train(Class ~ Housing.Rent + 
                                    Housing.Own + 
                                    Housing.ForFree, 
                                    data = germanTrain, method = "knn")
predict(knn_model_Housing, germanTest)
germanTestPredictions_Housing <- predict(knn_model_Housing, germanTest)
confusionMatrix(germanTestPredictions_Housing, germanTest$Class)


# Round = 2

train(Class ~ Duration + 
              Amount, 
              data = germanTrain,
              method = "knn")
knn_model_Duration_Amount <- train(Class ~ Duration + 
                                            Amount, 
                                            data = germanTrain,
                                            method = "knn")
predict(knn_model_Duration_Amount, germanTest)
germanTestPredictions_Duration_Amount <- predict(knn_model_Duration_Amount, germanTest)
confusionMatrix(germanTestPredictions_Duration_Amount, germanTest$Class)

train(Class ~ Duration + 
              InstallmentRatePercentage,
              data = germanTrain, 
              method = "knn")
knn_model_Duration_InstallmentRatePercentage <- train(Class ~ Duration + 
                                                              InstallmentRatePercentage,
                                                              data = germanTrain, 
                                                              method = "knn")
predict(knn_model_Duration_InstallmentRatePercentage, germanTest)
germanTestPredictions_Duration_InstallmentRatePercentage <- predict(knn_model_Duration_InstallmentRatePercentage, germanTest)
confusionMatrix(germanTestPredictions_Duration_InstallmentRatePercentage, germanTest$Class)

train(Class ~ Duration + 
              Age, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_Age <- train(Class ~ Duration + 
                                        Age, 
                                        data = germanTrain, 
                                        method = "knn")
predict(knn_model_Duration_Age, germanTest)
germanTestPredictions_Duration_Age <- predict(knn_model_Duration_Age, germanTest)
confusionMatrix(germanTestPredictions_Duration_Age, germanTest$Class)

train(Class ~ Duration + 
              NumberExistingCredits, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberExistingCredits <- train(Class ~ Duration + 
                                                          NumberExistingCredits, 
                                                          data = germanTrain, 
                                                          method = "knn")
predict(knn_model_Duration_NumberExistingCredits, germanTest)
germanTestPredictions_Duration_NumberExistingCredits <- predict(knn_model_Duration_NumberExistingCredits, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberExistingCredits, germanTest$Class)

train(Class ~ Duration + 
              NumberPeopleMaintenance, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance <- train(Class ~  Duration + 
                                                              NumberPeopleMaintenance, 
                                                              data = germanTrain, 
                                                              method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance <- predict(knn_model_Duration_NumberPeopleMaintenance, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance, germanTest$Class)

train(Class ~ Duration + 
              Telephone, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_Telephone <- train(Class ~ Duration + 
                                              Telephone, 
                                              data = germanTrain, 
                                              method = "knn")
predict(knn_model_Duration_Telephone, germanTest)
germanTestPredictions_Duration_Telephone <- predict(knn_model_Duration_Telephone, germanTest)
confusionMatrix(germanTestPredictions_Duration_Telephone, germanTest$Class)

train(Class ~ Duration + 
              ForeignWorker, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_ForeignWorker <- train(Class ~ Duration + 
                                                  ForeignWorker, 
                                                  data = germanTrain, 
                                                  method = "knn")
predict(knn_model_Duration_ForeignWorker, germanTest)
germanTestPredictions_Duration_ForeignWorker <- predict(knn_model_Duration_ForeignWorker, germanTest)
confusionMatrix(germanTestPredictions_Duration_ForeignWorker, germanTest$Class)

train(Class ~ Duration + 
              CheckingAccountStatus.lt.0 + 
              CheckingAccountStatus.0.to.200 + 
              CheckingAccountStatus.gt.200 + 
              CheckingAccountStatus.none, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_CheckingAccountStatus <- train(Class ~ Duration + 
                                                          CheckingAccountStatus.lt.0 + 
                                                          CheckingAccountStatus.0.to.200 + 
                                                          CheckingAccountStatus.gt.200 + 
                                                          CheckingAccountStatus.none, 
                                                          data = germanTrain, 
                                                          method = "knn")
predict(knn_model_Duration_CheckingAccountStatus, germanTest)
germanTestPredictions_Duration_CheckingAccountStatus <- predict(knn_model_Duration_CheckingAccountStatus, germanTest)
confusionMatrix(germanTestPredictions_Duration_CheckingAccountStatus, germanTest$Class)

train(Class ~ Duration + 
              CreditHistory.NoCredit.AllPaid + 
              CreditHistory.ThisBank.AllPaid + 
              CreditHistory.PaidDuly + 
              CreditHistory.Delay + 
              CreditHistory.Critical, 
              data = germanTrain, 
              method = "knn")
knnmodel_Duration_CreditHistory <- train(Class ~ Duration + 
                                                  CreditHistory.NoCredit.AllPaid + 
                                                  CreditHistory.ThisBank.AllPaid + 
                                                  CreditHistory.PaidDuly + 
                                                  CreditHistory.Delay + 
                                                  CreditHistory.Critical, 
                                                  data = germanTrain, 
                                                  method = "knn")
predict(knnmodel_Duration_CreditHistory, germanTest)
germanTestPredictions_Duration_CreditHistory <- predict(knnmodel_Duration_CreditHistory, germanTest)
confusionMatrix(germanTestPredictions_Duration_CreditHistory, germanTest$Class)

train(Class ~ Duration + 
              SavingsAccountBonds.lt.100 + 
              SavingsAccountBonds.100.to.500 + 
              SavingsAccountBonds.500.to.1000 + 
              SavingsAccountBonds.gt.1000 + 
              SavingsAccountBonds.Unknown, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_SavingsAccountBonds <- train(Class ~ Duration + 
                                                        SavingsAccountBonds.lt.100 + 
                                                        SavingsAccountBonds.100.to.500 + 
                                                        SavingsAccountBonds.500.to.1000 + 
                                                        SavingsAccountBonds.gt.1000 + 
                                                        SavingsAccountBonds.Unknown, 
                                                        data = germanTrain, 
                                                        method = "knn")
predict(knn_model_Duration_SavingsAccountBonds, germanTest)
germanTestPredictions_Duration_SavingsAccountBonds <- predict(knn_model_Duration_SavingsAccountBonds, germanTest)
confusionMatrix(germanTestPredictions_Duration_SavingsAccountBonds, germanTest$Class)


train(Class ~ Duration +
              EmploymentDuration.lt.1 + 
              EmploymentDuration.1.to.4 + 
              EmploymentDuration.4.to.7 + 
              EmploymentDuration.gt.7 + 
              EmploymentDuration.Unemployed, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_EmploymentDuration <- train(Class ~ Duration + 
                                                        EmploymentDuration.lt.1 + 
                                                        EmploymentDuration.1.to.4 + 
                                                        EmploymentDuration.4.to.7 + 
                                                        EmploymentDuration.gt.7 + 
                                                        EmploymentDuration.Unemployed, 
                                                        data = germanTrain, 
                                                        method = "knn")
predict(knn_model_Duration_EmploymentDuration, germanTest)
germanTestPredictions_Duration_EmploymentDuration <- predict(knn_model_Duration_EmploymentDuration, germanTest)
confusionMatrix(germanTestPredictions_Duration_EmploymentDuration, germanTest$Class)


train(Class ~ Duration + 
              Personal.Male.Divorced.Seperated + 
              Personal.Female.NotSingle + 
              Personal.Male.Single + 
              Personal.Male.Married.Widowed + 
              Personal.Female.Single,
              data = germanTrain, method = "knn")
knn_model_Duration_Personal <- train(Class ~ Duration + 
                                             Personal.Male.Divorced.Seperated + 
                                             Personal.Female.NotSingle + 
                                             Personal.Male.Single + 
                                             Personal.Male.Married.Widowed +
                                             Personal.Female.Single, 
                                             data = germanTrain,
                                             method = "knn")
predict(knn_model_Duration_Personal, germanTest)
germanTestPredictions_Duration_Personal <- predict(knn_model_Duration_Personal, germanTest)
confusionMatrix(germanTestPredictions_Duration_Personal, germanTest$Class)


train(Class ~  Duration + 
               Property.RealEstate + 
               Property.Insurance +
               Property.CarOther + 
               Property.Unknown, 
               data = germanTrain, 
               method = "knn")
knn_model_Duration_Property <- train(Class ~ Duration + 
                                             Property.RealEstate + 
                                             Property.Insurance + 
                                             Property.CarOther +
                                             Property.Unknown,
                                             data = germanTrain, 
                                             method = "knn")
predict(knn_model_Duration_Property, germanTest)
germanTestPredictions_Duration_Property <- predict(knn_model_Duration_Property, germanTest)
confusionMatrix(germanTestPredictions_Duration_Property, germanTest$Class)


train(Class ~Duration + 
              Housing.Rent +
              Housing.Own +
              Housing.ForFree, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_Housing <- train(Class ~ Duration + 
                                            Housing.Rent +
                                            Housing.Own +
                                            Housing.ForFree, 
                                            data = germanTrain, 
                                            method = "knn")
predict(knn_model_Duration_Housing, germanTest)
germanTestPredictions_Duration_Housing <- predict(knn_model_Duration_Housing, germanTest)
confusionMatrix(germanTestPredictions_Duration_Housing, germanTest$Class)


# Round 3


train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              Amount, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Amount <- train(Class ~ Duration + 
                                                                    NumberPeopleMaintenance + 
                                                                    Amount, 
                                                                    data = germanTrain, 
                                                                    method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Amount, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Amount <- predict(knn_model_Duration_NumberPeopleMaintenance_Amount, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Amount, germanTest$Class)

train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              InstallmentRatePercentage, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_InstallmentRatePercentage <- train(Class ~ Duration + 
                                                                                      NumberPeopleMaintenance + 
                                                                                      InstallmentRatePercentage, 
                                                                                      data = germanTrain, 
                                                                                      method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_InstallmentRatePercentage, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_InstallmentRatePercentage <- predict(knn_model_Duration_NumberPeopleMaintenance_InstallmentRatePercentage, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_InstallmentRatePercentage, germanTest$Class)

train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              Age, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Age <- train(Class ~ Duration + 
                                                                NumberPeopleMaintenance + 
                                                                Age, 
                                                                data = germanTrain, 
                                                                method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Age, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Age <- predict(knn_model_Duration_NumberPeopleMaintenance_Age, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Age, germanTest$Class)

train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              NumberExistingCredits, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_NumberExistingCredits <- train(Class ~ Duration + 
                                                                                  NumberPeopleMaintenance + 
                                                                                  NumberExistingCredits, 
                                                                                  data = germanTrain, 
                                                                                  method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_NumberExistingCredits, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_NumberExistingCredits <- predict(knn_model_Duration_NumberPeopleMaintenance_NumberExistingCredits, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_NumberExistingCredits, germanTest$Class)

train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              Telephone, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Telephone <- train(Class ~ Duration + 
                                                                      NumberPeopleMaintenance + 
                                                                      Telephone, 
                                                                      data = germanTrain, 
                                                                      method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Telephone, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Telephone <- predict(knn_model_Duration_NumberPeopleMaintenance_Telephone, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Telephone, germanTest$Class)

train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              ForeignWorker, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_ForeignWorker <- train(Class ~ Duration + 
                                                                            NumberPeopleMaintenance + 
                                                                            ForeignWorker, 
                                                                            data = germanTrain, 
                                                                            method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_ForeignWorker, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_ForeignWorker <- predict(knn_model_Duration_NumberPeopleMaintenance_ForeignWorker, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_ForeignWorker, germanTest$Class)

train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              CheckingAccountStatus.lt.0 + 
              CheckingAccountStatus.0.to.200 + 
              CheckingAccountStatus.gt.200 +
              CheckingAccountStatus.none, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_CheckingAccountStatus <- train(Class ~ Duration + 
                                                                                  NumberPeopleMaintenance + 
                                                                                  CheckingAccountStatus.lt.0 + 
                                                                                  CheckingAccountStatus.0.to.200 + 
                                                                                  CheckingAccountStatus.gt.200 +
                                                                                  CheckingAccountStatus.none, 
                                                                                  data = germanTrain, 
                                                                                  method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_CheckingAccountStatus, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_CheckingAccountStatus <- predict(knn_model_Duration_NumberPeopleMaintenance_CheckingAccountStatus, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_CheckingAccountStatus, germanTest$Class)


train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              CreditHistory.NoCredit.AllPaid + 
              CreditHistory.ThisBank.AllPaid + 
              CreditHistory.PaidDuly + 
              CreditHistory.Delay + 
              CreditHistory.Critical, 
              data = germanTrain, 
              method = "knn")
knnmodel_Duration_NumberPeopleMaintenance_CreditHistory <- train(Class ~ Duration + 
                                                                          NumberPeopleMaintenance + 
                                                                          CreditHistory.NoCredit.AllPaid + 
                                                                          CreditHistory.ThisBank.AllPaid + 
                                                                          CreditHistory.PaidDuly + 
                                                                          CreditHistory.Delay + 
                                                                          CreditHistory.Critical, 
                                                                          data = germanTrain, 
                                                                          method = "knn")
predict(knnmodel_Duration_NumberPeopleMaintenance_CreditHistory, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_CreditHistory <- predict(knnmodel_Duration_NumberPeopleMaintenance_CreditHistory, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_CreditHistory, germanTest$Class)

train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              SavingsAccountBonds.lt.100 + 
              SavingsAccountBonds.100.to.500 + 
              SavingsAccountBonds.500.to.1000 + 
              SavingsAccountBonds.gt.1000 + 
              SavingsAccountBonds.Unknown,
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_SavingsAccountBonds <- train(Class ~ Duration + 
                                                                                NumberPeopleMaintenance + 
                                                                                SavingsAccountBonds.lt.100 + 
                                                                                SavingsAccountBonds.100.to.500 + 
                                                                                SavingsAccountBonds.500.to.1000 + 
                                                                                SavingsAccountBonds.gt.1000 + 
                                                                                SavingsAccountBonds.Unknown,
                                                                                data = germanTrain, 
                                                                                method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_SavingsAccountBonds, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_SavingsAccountBonds <- predict(knn_model_Duration_NumberPeopleMaintenance_SavingsAccountBonds, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_SavingsAccountBonds, germanTest$Class)


train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              EmploymentDuration.lt.1 + 
              EmploymentDuration.1.to.4 + 
              EmploymentDuration.4.to.7 + 
              EmploymentDuration.gt.7 +
              EmploymentDuration.Unemployed, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_EmploymentDuration <- train(Class ~ Duration + 
                                                                                NumberPeopleMaintenance + 
                                                                                EmploymentDuration.lt.1 + 
                                                                                EmploymentDuration.1.to.4 + 
                                                                                EmploymentDuration.4.to.7 + 
                                                                                EmploymentDuration.gt.7 +
                                                                                EmploymentDuration.Unemployed, 
                                                                                data = germanTrain, 
                                                                                method = "knn")
knn_model_Duration_NumberPeople
predict(knn_model_Duration_NumberPeopleMaintenance_EmploymentDuration, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_EmploymentDuration <- predict(knn_model_Duration_NumberPeopleMaintenance_EmploymentDuration, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_EmploymentDuration, germanTest$Class)


train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              Personal.Male.Divorced.Seperated + 
              Personal.Female.NotSingle + 
              Personal.Male.Single + 
              Personal.Male.Married.Widowed +
              Personal.Female.Single, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Personal <- train(Class ~ Duration + 
                                                                    NumberPeopleMaintenance + 
                                                                    Personal.Male.Divorced.Seperated + 
                                                                    Personal.Female.NotSingle + 
                                                                    Personal.Male.Single + 
                                                                    Personal.Male.Married.Widowed +
                                                                    Personal.Female.Single, 
                                                                    data = germanTrain, 
                                                                    method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Personal, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Personal <- predict(knn_model_Duration_NumberPeopleMaintenance_Personal, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Personal, germanTest$Class)


train(Class ~  Duration +
                NumberPeopleMaintenance + 
                Property.RealEstate + 
                Property.Insurance + 
                Property.CarOther + 
                Property.Unknown, 
                data = germanTrain, 
                method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Property <- train(Class ~ Duration +
                                                                      NumberPeopleMaintenance + 
                                                                      Property.RealEstate + 
                                                                      Property.Insurance + 
                                                                      Property.CarOther + 
                                                                      Property.Unknown, 
                                                                      data = germanTrain, 
                                                                      method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Property, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Property <- predict(knn_model_Duration_NumberPeopleMaintenance_Property, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Property, germanTest$Class)


train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              Housing.Rent +
              Housing.Own + 
              Housing.ForFree, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Housing <- train(Class ~ Duration + 
                                                                    NumberPeopleMaintenance + 
                                                                    Housing.Rent +
                                                                    Housing.Own + 
                                                                    Housing.ForFree, 
                                                                    data = germanTrain, 
                                                                    method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Housing, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing <- predict(knn_model_Duration_NumberPeopleMaintenance_Housing, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing, germanTest$Class)



# Round 4


train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              Housing.Rent + 
              Housing.Own + 
              Housing.ForFree + 
              Amount, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Housing_Amount <- train(Class ~ Duration + 
                                                                            NumberPeopleMaintenance + 
                                                                            Housing.Rent + 
                                                                            Housing.Own + 
                                                                            Housing.ForFree + 
                                                                            Amount, 
                                                                            data = germanTrain, 
                                                                            method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Amount, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Amount <- predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Amount, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Amount, germanTest$Class)

train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              Housing.Rent + 
              Housing.Own + 
              Housing.ForFree +
              InstallmentRatePercentage, 
              data = germanTrain,
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Housing_InstallmentRatePercentage <- train(Class ~ Duration + 
                                                                                              NumberPeopleMaintenance + 
                                                                                              Housing.Rent + 
                                                                                              Housing.Own + 
                                                                                              Housing.ForFree +
                                                                                              InstallmentRatePercentage, 
                                                                                              data = germanTrain,
                                                                                              method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Housing_InstallmentRatePercentage, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_InstallmentRatePercentage <- predict(knn_model_Duration_NumberPeopleMaintenance_Housing_InstallmentRatePercentage, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_InstallmentRatePercentage, germanTest$Class)

train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              Housing.Rent + 
              Housing.Own + 
              Housing.ForFree +
              Age, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Housing_Age <- train(Class ~ Duration + 
                                                                        NumberPeopleMaintenance + 
                                                                        Housing.Rent + 
                                                                        Housing.Own + 
                                                                        Housing.ForFree +
                                                                        Age, 
                                                                        data = germanTrain, 
                                                                        method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Age, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Age <- predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Age, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Age, germanTest$Class)

train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              Housing.Rent + 
              Housing.Own +
              Housing.ForFree + 
              NumberExistingCredits, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Housing_NumberExistingCredits <- train(Class ~ Duration + 
                                                                                          NumberPeopleMaintenance + 
                                                                                          Housing.Rent + 
                                                                                          Housing.Own +
                                                                                          Housing.ForFree + 
                                                                                          NumberExistingCredits, 
                                                                                          data = germanTrain, 
                                                                                          method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Housing_NumberExistingCredits, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_NumberExistingCredits <- predict(knn_model_Duration_NumberPeopleMaintenance_Housing_NumberExistingCredits, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_NumberExistingCredits, germanTest$Class)

train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              Housing.Rent + 
              Housing.Own + 
              Housing.ForFree + 
              Telephone, 
              data = germanTrain,
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone <- train(Class ~  Duration + 
                                                                                NumberPeopleMaintenance + 
                                                                                Housing.Rent + 
                                                                                Housing.Own + 
                                                                                Housing.ForFree + 
                                                                                Telephone, 
                                                                                data = germanTrain,
                                                                                method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone <- predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone, germanTest$Class)

train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              Housing.Rent + 
              Housing.Own + 
              Housing.ForFree +
              ForeignWorker, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Housing_ForeignWorker <- train(Class ~ Duration + 
                                                                                  NumberPeopleMaintenance + 
                                                                                  Housing.Rent + 
                                                                                  Housing.Own + 
                                                                                  Housing.ForFree +
                                                                                  ForeignWorker, 
                                                                                  data = germanTrain, 
                                                                                  method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Housing_ForeignWorker, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_ForeignWorker <- predict(knn_model_Duration_NumberPeopleMaintenance_Housing_ForeignWorker, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_ForeignWorker, germanTest$Class)


train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              Housing.Rent + 
              Housing.Own +
              Housing.ForFree + 
              CheckingAccountStatus.lt.0 + 
              CheckingAccountStatus.0.to.200 + 
              CheckingAccountStatus.gt.200 + 
              CheckingAccountStatus.none, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Housing_CheckingAccountStatus <- train(Class ~ Duration + 
                                                                                          NumberPeopleMaintenance + 
                                                                                          Housing.Rent + 
                                                                                          Housing.Own +
                                                                                          Housing.ForFree + 
                                                                                          CheckingAccountStatus.lt.0 + 
                                                                                          CheckingAccountStatus.0.to.200 + 
                                                                                          CheckingAccountStatus.gt.200 + 
                                                                                          CheckingAccountStatus.none, 
                                                                                          data = germanTrain, 
                                                                                          method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Housing_CheckingAccountStatus, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_CheckingAccountStatus <- predict(knn_model_Duration_NumberPeopleMaintenance_Housing_CheckingAccountStatus, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_CheckingAccountStatus, germanTest$Class)


train(Class ~ Duration +
              NumberPeopleMaintenance + 
              Housing.Rent + 
              Housing.Own + 
              Housing.ForFree + 
              CreditHistory.NoCredit.AllPaid +
              CreditHistory.ThisBank.AllPaid + 
              CreditHistory.PaidDuly + 
              CreditHistory.Delay + 
              CreditHistory.Critical,
              data = germanTrain,
              method = "knn")
knnmodel_Duration_NumberPeopleMaintenance_Housing_CreditHistory <- train(Class ~ Duration +
                                                                                  NumberPeopleMaintenance + 
                                                                                  Housing.Rent + 
                                                                                  Housing.Own + 
                                                                                  Housing.ForFree + 
                                                                                  CreditHistory.NoCredit.AllPaid +
                                                                                  CreditHistory.ThisBank.AllPaid + 
                                                                                  CreditHistory.PaidDuly + 
                                                                                  CreditHistory.Delay + 
                                                                                  CreditHistory.Critical,
                                                                                  data = germanTrain,
                                                                                  method = "knn")
predict(knnmodel_Duration_NumberPeopleMaintenance_Housing_CreditHistory, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_CreditHistory <- predict(knnmodel_Duration_NumberPeopleMaintenance_Housing_CreditHistory, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_CreditHistory, germanTest$Class)

train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              Housing.Rent + 
              Housing.Own + 
              Housing.ForFree + 
              SavingsAccountBonds.lt.100 +
              SavingsAccountBonds.100.to.500 + 
              SavingsAccountBonds.500.to.1000 + 
              SavingsAccountBonds.gt.1000 + 
              SavingsAccountBonds.Unknown, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Housing_SavingsAccountBonds <- train(Class ~ Duration + 
                                                                                        NumberPeopleMaintenance + 
                                                                                        Housing.Rent + 
                                                                                        Housing.Own + 
                                                                                        Housing.ForFree + 
                                                                                        SavingsAccountBonds.lt.100 +
                                                                                        SavingsAccountBonds.100.to.500 + 
                                                                                        SavingsAccountBonds.500.to.1000 + 
                                                                                        SavingsAccountBonds.gt.1000 + 
                                                                                        SavingsAccountBonds.Unknown, 
                                                                                        data = germanTrain, 
                                                                                        method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Housing_SavingsAccountBonds, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_SavingsAccountBonds <- predict(knn_model_Duration_NumberPeopleMaintenance_Housing_SavingsAccountBonds, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_SavingsAccountBonds, germanTest$Class)


train(Class ~ Duration + 
              NumberPeopleMaintenance +  
              Housing.Rent +
              Housing.Own +
              Housing.ForFree + 
              EmploymentDuration.lt.1 +
              EmploymentDuration.1.to.4 +
              EmploymentDuration.4.to.7 +
              EmploymentDuration.gt.7 +
              EmploymentDuration.Unemployed,
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Housing_EmploymentDuration <- train(Class ~ Duration + 
                                                                                        NumberPeopleMaintenance +  
                                                                                        Housing.Rent +
                                                                                        Housing.Own +
                                                                                        Housing.ForFree + 
                                                                                        EmploymentDuration.lt.1 +
                                                                                        EmploymentDuration.1.to.4 +
                                                                                        EmploymentDuration.4.to.7 +
                                                                                        EmploymentDuration.gt.7 +
                                                                                        EmploymentDuration.Unemployed,
                                                                                        data = germanTrain, 
                                                                                        method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Housing_EmploymentDuration, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_EmploymentDuration <- predict(knn_model_Duration_NumberPeopleMaintenance_Housing_EmploymentDuration, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_EmploymentDuration, germanTest$Class)


train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              Housing.Rent +
              Housing.Own + 
              Housing.ForFree +
              Personal.Male.Divorced.Seperated + 
              Personal.Female.NotSingle + 
              Personal.Male.Single +
              Personal.Male.Married.Widowed + 
              Personal.Female.Single,
              data = germanTrain,
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Housing_Personal <- train(Class ~Duration + 
                                                                            NumberPeopleMaintenance + 
                                                                            Housing.Rent +
                                                                            Housing.Own + 
                                                                            Housing.ForFree +
                                                                            Personal.Male.Divorced.Seperated + 
                                                                            Personal.Female.NotSingle + 
                                                                            Personal.Male.Single +
                                                                            Personal.Male.Married.Widowed + 
                                                                            Personal.Female.Single,
                                                                            data = germanTrain,
                                                                            method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Personal, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Personal <- predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Personal, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Personal, germanTest$Class)


train(Class ~  Duration + 
              NumberPeopleMaintenance +  
              Housing.Rent +
              Housing.Own +
              Housing.ForFree + 
              Property.RealEstate +
              Property.Insurance +
              Property.CarOther + 
              Property.Unknown, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Housing_Property <- train(Class ~ Duration + 
                                                                              NumberPeopleMaintenance +  
                                                                              Housing.Rent +
                                                                              Housing.Own +
                                                                              Housing.ForFree + 
                                                                              Property.RealEstate +
                                                                              Property.Insurance +
                                                                              Property.CarOther + 
                                                                              Property.Unknown, 
                                                                              data = germanTrain, 
                                                                              method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Property, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Property <- predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Property, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Property, germanTest$Class)


#Rount 5

train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              Housing.Rent + Housing.Own +
              Housing.ForFree + 
              Telephone + 
              Amount, 
              data = germanTrain,
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_Amount <- train(Class ~ Duration + 
                                                                                      NumberPeopleMaintenance + 
                                                                                      Housing.Rent + Housing.Own +
                                                                                      Housing.ForFree + 
                                                                                      Telephone + 
                                                                                      Amount, 
                                                                                      data = germanTrain,
                                                                                      method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_Amount, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone_Amount <- predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_Amount, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone_Amount, germanTest$Class)

train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              Housing.Rent +
              Housing.Own +
              Housing.ForFree + 
              Telephone +
              InstallmentRatePercentage,
              data = germanTrain,
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_InstallmentRatePercentage <- train(Class ~ Duration + 
                                                                                                        NumberPeopleMaintenance + 
                                                                                                        Housing.Rent +
                                                                                                        Housing.Own +
                                                                                                        Housing.ForFree + 
                                                                                                        Telephone +
                                                                                                        InstallmentRatePercentage,
                                                                                                        data = germanTrain,
                                                                                                        method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_InstallmentRatePercentage, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone_InstallmentRatePercentage <- predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_InstallmentRatePercentage, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone_InstallmentRatePercentage, germanTest$Class)

train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              Housing.Rent + 
              Housing.Own +
              Housing.ForFree + 
              Telephone +
              Age, 
              data = germanTrain,
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_Age <- train(Class ~ Duration + 
                                                                                  NumberPeopleMaintenance + 
                                                                                  Housing.Rent + 
                                                                                  Housing.Own +
                                                                                  Housing.ForFree + 
                                                                                  Telephone +
                                                                                  Age, 
                                                                                  data = germanTrain,
                                                                                  method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_Age, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone_Age <- predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_Age, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone_Age, germanTest$Class)

train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              Housing.Rent + 
              Housing.Own +
              Housing.ForFree + 
              Telephone +
              NumberExistingCredits,
              data = germanTrain,
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_NumberExistingCredits <- train(Class ~ Duration + 
                                                                                                    NumberPeopleMaintenance + 
                                                                                                    Housing.Rent + 
                                                                                                    Housing.Own +
                                                                                                    Housing.ForFree + 
                                                                                                    Telephone +
                                                                                                    NumberExistingCredits,
                                                                                                    data = germanTrain,
                                                                                                    method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_NumberExistingCredits, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone_NumberExistingCredits <- predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_NumberExistingCredits, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone_NumberExistingCredits, germanTest$Class)

train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              Housing.Rent +
              Housing.Own + 
              Housing.ForFree + 
              Telephone +
              ForeignWorker,
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_ForeignWorker <- train(Class ~ Duration + 
                                                                                            NumberPeopleMaintenance + 
                                                                                            Housing.Rent +
                                                                                            Housing.Own + 
                                                                                            Housing.ForFree + 
                                                                                            Telephone +
                                                                                            ForeignWorker,
                                                                                            data = germanTrain, 
                                                                                            method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_ForeignWorker, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone_ForeignWorker <- predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_ForeignWorker, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone_ForeignWorker, germanTest$Class)

train(Class ~ Duration + 
              NumberPeopleMaintenance +
              Housing.Rent + 
              Housing.Own + 
              Housing.ForFree +
              Telephone + 
              CheckingAccountStatus.lt.0 + 
              CheckingAccountStatus.0.to.200 + 
              CheckingAccountStatus.gt.200 +
              CheckingAccountStatus.none,
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_CheckingAccountStatus <- train(Class ~ Duration + 
                                                                                                    NumberPeopleMaintenance +
                                                                                                    Housing.Rent + 
                                                                                                    Housing.Own + 
                                                                                                    Housing.ForFree +
                                                                                                    Telephone + 
                                                                                                    CheckingAccountStatus.lt.0 + 
                                                                                                    CheckingAccountStatus.0.to.200 + 
                                                                                                    CheckingAccountStatus.gt.200 +
                                                                                                    CheckingAccountStatus.none,
                                                                                                    data = germanTrain, 
                                                                                                    method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_CheckingAccountStatus, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone_CheckingAccountStatus <- predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_CheckingAccountStatus, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone_CheckingAccountStatus, germanTest$Class)


train(Class ~ Duration + 
              NumberPeopleMaintenance + 
              Housing.Rent + 
              Housing.Own +
              Housing.ForFree + 
              Telephone + 
              CreditHistory.NoCredit.AllPaid +
              CreditHistory.ThisBank.AllPaid +
              CreditHistory.PaidDuly +
              CreditHistory.Delay + 
              CreditHistory.Critical, 
              data = germanTrain, 
              method = "knn")
knnmodel_Duration_NumberPeopleMaintenance_Housing_Telephone_CreditHistory <- train(Class ~ Duration + 
                                                                                            NumberPeopleMaintenance + 
                                                                                            Housing.Rent + 
                                                                                            Housing.Own +
                                                                                            Housing.ForFree + 
                                                                                            Telephone + 
                                                                                            CreditHistory.NoCredit.AllPaid +
                                                                                            CreditHistory.ThisBank.AllPaid +
                                                                                            CreditHistory.PaidDuly +
                                                                                            CreditHistory.Delay + 
                                                                                            CreditHistory.Critical, 
                                                                                            data = germanTrain, 
                                                                                            method = "knn")
predict(knnmodel_Duration_NumberPeopleMaintenance_Housing_Telephone_CreditHistory, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone_CreditHistory <- predict(knnmodel_Duration_NumberPeopleMaintenance_Housing_Telephone_CreditHistory, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone_CreditHistory, germanTest$Class)

train(Class ~ Duration + 
              NumberPeopleMaintenance +
              Housing.Rent + 
              Housing.Own + 
              Housing.ForFree + 
              Telephone + 
              SavingsAccountBonds.lt.100 +
              SavingsAccountBonds.100.to.500 + 
              SavingsAccountBonds.500.to.1000 + 
              SavingsAccountBonds.gt.1000 +
              SavingsAccountBonds.Unknown, 
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_SavingsAccountBonds <- train(Class ~ Duration + 
                                                                                                  NumberPeopleMaintenance +
                                                                                                  Housing.Rent + 
                                                                                                  Housing.Own + 
                                                                                                  Housing.ForFree + 
                                                                                                  Telephone + 
                                                                                                  SavingsAccountBonds.lt.100 +
                                                                                                  SavingsAccountBonds.100.to.500 + 
                                                                                                  SavingsAccountBonds.500.to.1000 + 
                                                                                                  SavingsAccountBonds.gt.1000 +
                                                                                                  SavingsAccountBonds.Unknown, 
                                                                                                  data = germanTrain, 
                                                                                                  method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_SavingsAccountBonds, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone_SavingsAccountBonds <- predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_SavingsAccountBonds, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone_SavingsAccountBonds, germanTest$Class)


train(Class ~ Duration + 
              NumberPeopleMaintenance +  
              Housing.Rent +
              Housing.Own + 
              Housing.ForFree +
              Telephone + 
              EmploymentDuration.lt.1 +
              EmploymentDuration.1.to.4 +
              EmploymentDuration.4.to.7 +
              EmploymentDuration.gt.7 +
              EmploymentDuration.Unemployed,
              data = germanTrain, 
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_EmploymentDuration <- train(Class ~ Duration + 
                                                                                                  NumberPeopleMaintenance +  
                                                                                                  Housing.Rent +
                                                                                                  Housing.Own + 
                                                                                                  Housing.ForFree +
                                                                                                  Telephone + 
                                                                                                  EmploymentDuration.lt.1 +
                                                                                                  EmploymentDuration.1.to.4 +
                                                                                                  EmploymentDuration.4.to.7 +
                                                                                                  EmploymentDuration.gt.7 +
                                                                                                  EmploymentDuration.Unemployed,
                                                                                                  data = germanTrain, 
                                                                                                  method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_EmploymentDuration, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone_EmploymentDuration <- predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_EmploymentDuration, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone_EmploymentDuration, germanTest$Class)


train(Class ~ Duration + 
              NumberPeopleMaintenance +  
              Housing.Rent +
              Housing.Own + 
              Housing.ForFree +
              Telephone +
              Personal.Male.Divorced.Seperated + 
              Personal.Female.NotSingle +
              Personal.Male.Single + 
              Personal.Male.Married.Widowed + 
              Personal.Female.Single, 
              data = germanTrain,
              method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_Personal <- train(Class ~  Duration + 
                                                                                        NumberPeopleMaintenance +  
                                                                                        Housing.Rent +
                                                                                        Housing.Own + 
                                                                                        Housing.ForFree +
                                                                                        Telephone +
                                                                                        Personal.Male.Divorced.Seperated + 
                                                                                        Personal.Female.NotSingle +
                                                                                        Personal.Male.Single + 
                                                                                        Personal.Male.Married.Widowed + 
                                                                                        Personal.Female.Single, 
                                                                                        data = germanTrain,
                                                                                        method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_Personal, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone_Personal <- predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_Personal, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone_Personal, germanTest$Class)


train(Class ~  Duration + 
                NumberPeopleMaintenance +  
                Housing.Rent +
                Housing.Own + 
                Housing.ForFree +
                Telephone + 
                Property.RealEstate +
                Property.Insurance +
                Property.CarOther + 
                Property.Unknown,
                data = germanTrain,
                method = "knn")
knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_Property <- train(Class ~ Duration + 
                                                                                        NumberPeopleMaintenance +  
                                                                                        Housing.Rent +
                                                                                        Housing.Own + 
                                                                                        Housing.ForFree +
                                                                                        Telephone + 
                                                                                        Property.RealEstate +
                                                                                        Property.Insurance +
                                                                                        Property.CarOther + 
                                                                                        Property.Unknown,
                                                                                        data = germanTrain,
                                                                                        method = "knn")
predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_Property, germanTest)
germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone_Property <- predict(knn_model_Duration_NumberPeopleMaintenance_Housing_Telephone_Property, germanTest)
confusionMatrix(germanTestPredictions_Duration_NumberPeopleMaintenance_Housing_Telephone_Property, germanTest$Class)





















