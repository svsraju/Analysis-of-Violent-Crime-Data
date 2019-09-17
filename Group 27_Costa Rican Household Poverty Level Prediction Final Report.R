#libraries used
library(tidyverse)
library(dplyr)
library(Hmisc)
library(ggplot2)
library(caret)
library(plyr)
library(corrplot)
library(GGally)
library(corrr)
library(mlogit)
library(VGAM)
library(glmnet)
library(nnet)
library(gbm)
library(onehot)
library(MASS)
library(kernlab)
library(e1071)

ctrain <- read.csv("train.csv")
ctest <- read.csv("test.csv")
ctest$Target <- 0
View(ctrain)

#We can observe that several family members donot have the same target variable. We solve this by giving 
#all household members the same Target as that of head of house.
true_target <- ctrain[ctrain$parentesco1 == 1, c('idhogar', 'Target')]

for(n in nrow(true_target)){
  ctrain$Target[ctrain$idhogar == true_target[n, 'idhogar']] <- true_target[n, 'Target']
}

P_full <- ctrain

P_full$Target <- as.factor(P_full$Target)
#P_full$Target = mapvalues(P_full$Target , from = c("1","2","3","4"),
#                          to = c("extreme","moderate","decent","welloff"))

myfun<-function(x) mean(is.na(x))*100
apply(P_full[,c(colnames(P_full)[colSums(is.na(P_full)) > 0])],2,myfun) #Getting percentage of missing values in each variable
#variables with major missing valuea are....
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#As we can see in the SQDependency, we can see that no : 0 and yes: 1. And the values are numeric.
P_full$dependency = mapvalues(P_full$dependency , from = c("yes","no"),
                              to = c("1","0"))
P_full$dependency <- as.numeric(levels(P_full$dependency))[P_full$dependency]

str(P_full$dependency)

#edjefa and edjefe are transformed based on their squared column.
P_full$edjefa = mapvalues(P_full$edjefa , from = c("yes","no"),
                          to = c("1","0"))
P_full$edjefa <- as.numeric(levels(P_full$edjefa))[P_full$edjefa]

P_full$edjefe = mapvalues(P_full$edjefe , from = c("yes","no"),
                          to = c("1","0"))
P_full$edjefe <- as.numeric(levels(P_full$edjefe))[P_full$edjefe]


#Missing Values

#71.77% of monthly rent is missing. We'll give these value to zero for those who have own house or paying installments.
P_full$v2a1[(is.na(P_full$v2a1)) & (P_full$tipovivi1 == 1)] <- 0
P_full$v2a1[(is.na(P_full$v2a1)) & (P_full$tipovivi2 == 1)] <- 0

#There is NA in number of tablets if individual is not holding any tablets
P_full$v18q1[is.na(P_full$v18q1) & (P_full$v18q == 0)] <- 0

#rez_sec is made zero for those with less tahn 7 years and above 19 years age.
#rez_sec above 5 is made equal to 5.
P_full$rez_esc[(is.na(P_full$rez_esc)) & ((P_full$age < 7) | (P_full$age > 19))] <- 0
P_full$rez_esc[P_full$rez_esc > 5] = 5

P_full$meaneduc[(is.na(P_full$meaneduc)) & (P_full$age < 18)] <- 0

P_full$v2a1[is.na(P_full$v2a1)] <- getmode(P_full$v2a1)
P_full$rez_esc[is.na(P_full$rez_esc)] <- getmode(P_full$rez_esc)
P_full$meaneduc[is.na(P_full$meaneduc)] <- getmode(P_full$meaneduc)

hist(ctrain$Target[is.na(ctrain$v2a1)])
hist(ctrain$Target[is.na(ctrain$rez_esc)])

#The columns are mailnly categorized into 4 ways as ID columns, Individual, Household and squared columns.
?ggplot
squared_column <- c('SQBescolari', 'SQBage', 'SQBhogar_total', 'SQBedjefe',
                    'SQBhogar_nin', 'SQBovercrowding', 'SQBdependency',
                    'SQBmeaned')

#All the eight squared columns have the squared values of the other columns. 
#This clearly indicates that these columns will be highly correlated with their original columns.
#So, we'll remove these squared columns.
P_full[,squared_column] <- NULL

id_column <- c('Id', 'idhogar', 'Target')

ind_bool = c('v18q', 'dis', 'male', 'female', 'estadocivil1', 'estadocivil2', 'estadocivil3', 
             'estadocivil4', 'estadocivil5', 'estadocivil6', 'estadocivil7', 
             'parentesco1', 'parentesco2',  'parentesco3', 'parentesco4', 'parentesco5', 
             'parentesco6', 'parentesco7', 'parentesco8',  'parentesco9', 'parentesco10', 
             'parentesco11', 'parentesco12', 'instlevel1', 'instlevel2', 'instlevel3', 
             'instlevel4', 'instlevel5', 'instlevel6', 'instlevel7', 'instlevel8', 
             'instlevel9', 'mobilephone', 'rez_esc-missing')

ind_ordered = c('rez_esc', 'escolari', 'age')
head_bool = c('hacdor', 'hacapo', 'v14a', 'refrig', 'paredblolad', 'paredzocalo', 
              'paredpreb','pisocemento', 'pareddes', 'paredmad',
              'paredzinc', 'paredfibras', 'paredother', 'pisomoscer', 'pisoother', 
              'pisonatur', 'pisonotiene', 'pisomadera',
              'techozinc', 'techoentrepiso', 'techocane', 'techootro', 'cielorazo', 
              'abastaguadentro', 'abastaguafuera', 'abastaguano',
              'public', 'planpri', 'noelec', 'coopele', 'sanitario1', 
              'sanitario2', 'sanitario3', 'sanitario5',   'sanitario6',
              'energcocinar1', 'energcocinar2', 'energcocinar3', 'energcocinar4', 
              'elimbasu1', 'elimbasu2', 'elimbasu3', 'elimbasu4', 
              'elimbasu5', 'elimbasu6', 'epared1', 'epared2', 'epared3',
              'etecho1', 'etecho2', 'etecho3', 'eviv1', 'eviv2', 'eviv3', 
              'tipovivi1', 'tipovivi2', 'tipovivi3', 'tipovivi4', 'tipovivi5', 
              'computer', 'television', 'lugar1', 'lugar2', 'lugar3',
              'lugar4', 'lugar5', 'lugar6', 'area1', 'area2', 'v2a1-missing')

head_ordered = c('rooms', 'r4h1', 'r4h2', 'r4h3', 'r4m1','r4m2','r4m3', 'r4t1',  'r4t2', 
                 'r4t3', 'v18q1', 'tamhog','tamviv','hhsize','hogar_nin',
                 'hogar_adul','hogar_mayor','hogar_total',  'bedrooms', 'qmobilephone')

head_cont = c('v2a1', 'dependency', 'edjefe', 'edjefa', 'meaneduc', 'overcrowding')

#data containing only heads are seperated to check their behaviour.
heads <- ctrain[(ctrain$parentesco1 == 1),]
heads = heads[,c(id_column+head_bool+head_cont+head_ordered)]

corr1 <- c('r4t3', 'tamhog', 'tamviv', 'hhsize', 'hogar_total')
nums <- unlist(lapply(P_full[,c('r4t3', 'tamhog', 'tamviv', 'hhsize', 'hogar_total')], is.numeric))
c_corr <- P_full[,corr1]
corr_c <- cor(c_corr)
View(corr_c)
corrplot(corr_c, method = "square", tl.cex = 0.6, tl.offset = 0.4,tl.srt = 90, cl.ratio = 0.3)
?cor
ggcorr(P_full[, corr1])
#From the above correlation plot we can see that the columns 'hhsize','tamhog', 'hogar_total', 'r4t3' 
#are highly correlated. These are toatal persons in household, size of household, Number of individuals in household
#and household size. 
P_full[,c('tamhog', 'hogar_total', 'r4t3')] <- NULL

#area1, and area2 tell us if the individual live in Urban or Rural zones. Since, both represent one or the other, we'll remove one.
P_full$area2 <- NULL

#We'll ceate new feature that will both the negitives and positives of the house. 
#We add in negitive if there is no toilet, no electricity, no floor at house, if no water provision and if 
#house have no ceiling.
P_full$negitive <- 1 * (P_full$sanitario1 + (P_full$noelec == 1) + P_full$pisonotiene +
                          P_full$abastaguano + (P_full$cielorazo == 0))

#We add in positive if there is refrigirator, if they own tablet in the household, if they have telision and computer.
P_full$positive <- 1 * (P_full$refrig + (P_full$v18q1 > 0) + P_full$computer +
                          P_full$television)

ggplot(P_full, aes(x = P_full$Target, P_full$positive)) + geom_count(aes(col = P_full$Target)) + labs(title = "Luxuries count v/s Target",
                                                                                                      x = "Poverty Level", y = "Luxuries Count")
ggplot(P_full, aes(x = P_full$Target, P_full$negitive)) + geom_count(aes(col = P_full$Target)) + labs(title = "Luxuries count v/s Target",
                                                                                                      x = "Poverty Level", y = "Luxuries Count")

?ggplot
variables = c('Target', 'dependency', 'negitive', 'meaneduc',
              'r4m1', 'overcrowding')
ggcorr(P_full[, variables])

#Since there are two columns with male and female, we'll remove male.
P_full$male <- NULL
ggplot(ctrain, aes(x = ctrain$Target, y = ctrain$overcrowding)) + geom_boxplot(aes(group = Target, col = Target))

ggplot(ctrain, aes(x = ctrain$Target, y = ctrain$r4t3)) + geom_boxplot(aes(group = Target, col = Target))
ggplot(ctrain, aes(x = ctrain$Target, y = ctrain$hogar_nin)) + geom_boxplot(aes(group = Target, col = Target))
ggplot(ctrain, aes(x = ctrain$Target, y = ctrain$qmobilephone)) + geom_boxplot(aes(group = Target, col = Target))

#Id and idhogar are removed before modelling
P_full$Id <- NULL
P_full$idhogar <- NULL
#P_full$agesq <- NULL
ctrain <- P_full
ctest <- P_full[9558:33413,]
#==================================================================================#

set.seed(1)
train_index <- sample(1:nrow(ctrain), 7000)
crtrain <- ctrain[train_index,]
crtest <- ctrain[-train_index,]
crtrain$elimbasu5 <- NULL
crtest$elimbasu5 <- NULL

#==============================MULTINOMIAL LOGISTIC REGRESSION====================================================#

ml_fit <- multinom(Target~.,data = crtrain)
summary(ml_fit)
save(ml_fit,file =  "ml_fit.rda")

ml_pred <- predict(ml_fit, crtest)
head(ml_pred)
accuracy <- mean(ml_pred == crtest$Target)
accuracy
cm_ml <- confusionMatrix(ml_pred, crtest$Target)
#======================Gradient Boosting Method============================================================#

fitControl <- trainControl(## 01-fold CV
  method = "cv",
  number = 10)

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

set.seed(2)
gbmFit1 <- train(Target~., crtrain,  
                 method = "gbm", 
                 trControl = fitControl, 
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE, tuneGrid = gbmGrid)

save(gbmFit1,file =  "gbmFit1.rda")
summary(gbmFit1)
trellis.par.set(caretTheme())
plot(gbmFit1, metric = "Kappa")
gbmFit1
plot(gbmFit1, metric = "Kappa", plotType = "level",
     scales = list(x = list(rot = 90)))

gbm_predict <- predict(gbmFit1, newdata = crtest)
head(gbm_predict)
varImp(gbmFit1)
accuracy <- mean(gbm_predict == crtest$Target)
accuracy
cm_gbm <- confusionMatrix(gbm_predict, crtest$Target)

#==================================================================================#

# crtrain$Target <- as.factor(crtrain$Target)
# str(crtrain$Target)
# 
# ctrain$Target <- NULL
# encoder <- onehot(ctrain, stringsAsFactors = TRUE, addNA = FALSE,max_levels = 300 )
# 
# train_target <- crtrain$Target
# test_target <- crtest$Target
# crtrain$Target <- NULL
# crtest$Target <- NULL
# ?train
# x <- predict(encoder, crtrain)
# z <- predict(encoder, crtest)
# 
# crtrain$Target <- train_target
# gbm_fit <- gbm(Target~., data = crtrain)
# summary(gbm_fit)

#==================================================================================#

fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10)
set.seed(3)
svm_fit <- train(Target~., data = crtrain, method = "svmRadial",
                 trContrl = fitControl,
                 preprocess = c("center", "scale"))

save(svm_fit, file = "svm_fit.rda")
svm_fit
svm_pred <- predict(svm_fit, crtest)
head(svm_pred)
accuracy <- mean(svm_pred == crtest$Target)
accuracy
confusionMatrix(svm_pred, crtest$Target)


svmgrid <- expand.grid(sigma = c(.01, .015, 0.2),
                    C = c(0.75, 0.9, 1, 1.1, 1.25))

#Train and Tune the SVM
svm.tune <- train(Target~.,
                  crtrain,
                  method = "svmRadial",
                  preProc = c("center","scale"),
                  metric="kappa",
                  tuneGrid = grid,
                  trControl=fitControl, verbose= FALSE)

#==========================================================================================#
??sigest
sigmaRangeReduced <- sigest(Target~., data = crtrain)
svmRGridReduced <- expand.grid(sigma = sigmaRangeReduced[1],
                                  C = 2^(seq(-4, 4)))
fitControl <- trainControl(## 10-fold CV
  method = "cv", 
  number = 10)

set.seed(4)
svmRadial_fit <- train(Target~., crtrain,
                     method = "svmRadial",
                      metric = "kappa",
                      preProc = c("center", "scale"),
                      tuneGrid = svmRGridReduced,
                      fit = FALSE,
                   trControl = fitControl)
save(svmRadial_fit, file =  "svmRadial_fit.rda")
svmR_pred <- predict(svmRadial_fit, crtest)
accuracy <- mean(svmR_pred == crtest$Target)
accuracy
cm_svmR <- confusionMatrix(svmR_pred, crtest$Target)


#=============================================================================================#

knn_fit <- train(Target~., crtrain,
                 method = "knn", metric = "kappa",
                 preProc = c("center", "scale"),
                 tuneGrid = data.frame(.k = c(4*(0:5)+1,
                                              20*(1:5)+1,
                                              50*(2:9)+1)),
                 trControl = fitControl)

plot(knn_fit)
knn_fit
knn_predict <- predict(knn_fit, crtest)
varImp(knn_fit)
accuracy <- mean(knn_predict == crtest$Target)
accuracy
cm_knn <- confusionMatrix(knn_predict, crtest$Target)
save(knn_fit, file =  "knn_fit.rda")
#=============================================================================================#

nnetGrid <- expand.grid(.size = 1:10, .decay = c(0, .1, 1, 2))
maxSize <- max(nnetGrid$.size)
numWts <- 1*(maxSize * (7000+1) + maxSize + 1)
set.seed(5)
nnet_fit <- train(Target~., crtrain, 
                  
                  method = "nnet", metric = "kappa",
                  preProc = c("center", "scale", "spatialSign"),
                  tuneGrid = nnetGrid, trace = FALSE,
                  maxit = 2000, MaxNWts = numWts,
                  trControl = fitControl)
save(nnet_fit, file =  "nnet_fit.rda")
nnet_pred <- predict(nnet_fit, crtest)
cm_nnet <- confusionMatrix(nnet_pred, crtest$Target)

#=============================================================================================#

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10)
View(crtrain)
crtrain$agesq <- NULL
seed <- 7
metric <- "Accuracy"


tunegrid <- expand.grid(.mtry=c(1,15))
set.seed(seed)
rf_default1 <- train(Target~., data=crtrain, method="rf", metric= "kappa", tuneGrid=tunegrid, trControl=fitControl)
print(rf_default1)

save(rf_default,file ="randomForest.rda")

save(rf_default1,file ="randomForest.rda1")
load("randomForest.rda1")
rf_pred <- predict(rf_default1, crtest)
cm_rf <- confusionMatrix(rf_pred, crtest$Target)
rf_default1
#=============================================================================================#

resamps <- resamples(list(GBM = gbmFit1,
                          SVM = svmRadial_fit,
                          NNET = nnet_fit,
#                          RF = rf_default1, 
                          knn = knn_fit))
resamps
summary(resamps)

theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
bwplot(resamps, layout = c(2, 1))

trellis.par.set(caretTheme())
dotplot(resamps, metric = "Accuracy")
splom(resamps)

xmodel_compare <- data.frame(Model = c('ml',
                                      'Gradient Boosting',
                                      'SVM Radial',
                                      'KNN',
                                      'RF',
                                      'nnet'),
                            Accuracy = c(cm_ml$overall[1],
                                         cm_gbm$overall[1],
                                         cm_svmR$overall[1],
                                         cm_rf$overall[1],
                                         cm_knn$overall[1],
                                         
                                         cm_nnet$overall[1]))


ggplot(aes(x=Model, y=Accuracy), data=model_compare) +
  geom_bar(stat='identity', fill = 'blue') +
  ggtitle('Comparative Accuracy of Models on Cross-Validation Data') +
  xlab('Models') +
  ylab('Overall Accuracy')

parallelplot(resamps)
svmRadial_fit
svm_fit

#=============================================================================================#

load("knn_fit.rda")
load("ml_fit.rda")
load("nnet_fit.rda")
load("gbmFit1.rda")
load("randomForest.rda1")
load("svmRadial_fit.rda")
load("svm_fit.rda")
gbmFit1
