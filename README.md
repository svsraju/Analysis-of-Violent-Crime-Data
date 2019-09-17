Costa Rican Household Poverty Level Prediction
===

The project is aimed at predicting the poverty level of the people in Latin America. This pre-diction helps the social programs to focus on the poorest segment of the population. The data for this project is taken from the Kaggle competition in 2018, Costa Rican Household Poverty Level Prediction. The data is provided by Inter- American Development. This problem is a mul-ticlass classification problem with 4 target class variables each depicting a different poverty level and 142 predictor variables. The variables consist of integer variables, numerical variables, categorical variables.

----
Poverty is measured by comparing a person's or family's income to a set poverty threshold or minimum amount of income needed to cover basic needs. People whose income falls under their threshold are considered poor. Many social programs have a hard time making sure the right people are given enough aid. It’s especially tricky when a program focuses on the poorest segment of the population. The world’s poorest typically can’t provide the necessary income and expense records to prove that they qualify. 
In Latin America, one popular method uses an algorithm to verify income qualification. It’s called the Proxy Means Test (or PMT). With PMT, agencies use a model that considers a family’s observable household attributes like the material of their walls and ceiling, or the assets found in the home to clas-sify them and predict their level of need. While this is an improvement, accuracy remains a problem as the region’s population grows and poverty declines.
Beyond Costa Rica, many countries face this same problem of inaccurately assessing social need. Our objective is to identify which all households have highest need for social welfare assistance, the new algorithm could be implemented in other countries around the world.

---
Problem Definition
------
The objective is to predict poverty level on a household level. We are given data on the individual level with everyone having unique features but also information about their household. To create a dataset for the task, we'll have to perform some aggregations of the individual data for each household. Moreo-ver, we must make a prediction for every individual in the test set. 

------

Author 
---
**Venkata Subbaraju Sagi**

All known Bugs and fixes can be sent to subbaraju.v@ou.edu


Packages required for the project :
dplyr,
Hmisc,
ggplot2,
caret,
plyr,
corrplot,
GGally,
corrr,
mlogit,
VGAM,
glmnet,
nnet,
gbm,
onehot,
MASS,
kernlab,
e1071


-------
Platform used
---------
I have used R studio for the project, you can download the file directly which will be a .R file and you can make necessary changes 

-------
Data Description:
---
The training data consists of 130 integer columns, 8 float (numeric) columns, and 5 object columns. The integer columns probably represent Boolean variables (that take on either 0 or 1) or ordinal varia-bles with discrete ordered values. and four factor variables of predictor classes. The object columns might pose an issue because they cannot be fed directly into a machine learning model. The training data consists of 9557 records and test data consists of 23856 records. This is a supervised multi-class classification machine learning problem.
The Target values represent poverty levels as follows:
1 = extreme Poverty
2 = moderate Poverty
3 = vulnerable households
4 = non-vulnerable households

----
Exploratory Data Analysis 
--
Initially we examined every feature individually as well as its interactions with other predictors. The given training data consists 9557 records. The training data does have missing values, which needs to be imputed. We have checked all the relations between variables, if there is any correlation between them. Understood the variables and their effect on our target variable.

---
Explanation of Modeling Choice:
---

The first natural step in creating basic logistic model will help you to understand all of 143 predictor variables and this model was used as a stepping stone to other models. In random forest model we found the most significant variable in based of mean decrease in accuracy as well as contributing to re-duction in node impurity.  we are planning to do more models to improve accuracy and statistics like multinomial regression, random forest, Gradient Boost, K- nearest neighbor We split the training data into two parts with the ration of 70 and 30, first part is to train the model and second part of the data is to validate the model 

---
 Modeling Technique and its Strength, Weakness:
------
Multinomial Logistic regression helps to understand the coefficients and significance of the variables. The exponential of coefficients corresponds to odd ratios for the given factor.Multinomial Logistic re-gression requires that all variables are independent of each other if there is any correlation between var-iables and then the model will tend to overweight the significance of those variables 
Random forest run very fast, and it is good at dealing with unbalanced data. To do regression with Ran-dom Forest, it cannot predict beyond the range in the training data, and that there may be chances of over-fit datasets that are particularly noisy. But the best test of any algorithm is how well it works upon your own data set. We also chose this model because it helps us in finding the feature importance plot which is bases for the understanding the features and coming up with performing feature engineering. 
Classification algorithm performance depends mainly on the characteristics of data to be classified also there is no single classifier that works best on all problems. For Example, K-Nearest neighbors is sensi-tive to noisy attributes and unbalanced datasets. Our aim is to find the classifier that suits our dataset so, we will also try other classifiers and find the one which has better accuracy and performance. 

---
Data Pre-Processing: 
------
**•	Missing Values** 


One of the most important steps of exploratory data analysis is finding missing values in the data and determining how to handle them. Missing values must be filled in before we use a machine learning model and we need to think of the best strategy for filling them in based on the feature: this is where we'll have to start digging into the data definitions. We have checked the percentage of missing values in each column.
Columns "v2a1","v18q1","rez_esc","meaneduc","SQBmeaned" have missing values. While "meaneduc"    and "SQBmeaned" had very less percentage of the missingness. Other three variables have missingness              close to 75 % in them.

If we look at the data definitions, we see that v18q indicates whether a family owns a tablet. We should        investigate this column combined with the number of tablets, so we are comparing the data and checking    the value corresponding to the same row. If v18q is zero and the value corresponding to the same row is    missing for v18q, we are equating them to zero. All the Missing values are replaced with zeros as they          don’t have any tablets, it must be due to this reason, these fields were missing.
The next missing column is v2a1 which represents the monthly rent payment. In addition to looking at the missing values of the monthly rent payment, it will be interesting to also look at the distribution                           of tipovivi1 and tipovivi2, the columns showing the ownership/renting status of the home. the households that do not have a monthly rent payment generally own their own home or they are paying installments. In a few other situations, we are not sure of the reason for the missing information, so we have used mode         value imputation for replacing these values.

The last column with a high percentage of missing values is rez_esc indicating years behind in school. For the families with a null value, is possible that they have no children currently in school. Let's test this out   by finding the ages of those who have a missing value in this column and the ages of those who do not         have a missing value. rez_esc is made zero for those children with less than 7 years and above 19 years       age, as it must be that these children are not in school, rez_esc above 5 is made equal to 5. In a few other                          situations, we are not sure of the reason for the missing information, so we have used mode value                         imputation for replacing these values.

The remaining missing values in each column will be filled in using Imputation. There are several types of imputation commonly used, and one of the simplest and most effective methods is to fill in the missing values with the mode of the column. So, we did this mode imputation for other two columns.

**•	Skew Transformation**


The data is already normalized, so we did no encounter any skewness in the data.

**•	Outlier Analysis**


We have checked for any outliers present in the dataset, we think all these were treated for this dataset, so we don’t have any outliers in the dataset.

---
Data Exploration
------
Some columns seem to be a mix of strings and numbers which we'll need to address before doing any machine learning. According to the documentation dependency, edjefe and edjefa have mix of strings and numbers. edjefe and edjefa variables are years of education of male and female head of household respectively. All these three variables have a squared root column each, the data in those columns is same as square of the value present in these columns. So, comparing these values, for these three varia-bles we have replaced "yes" with 1 and "no" with 0. 
Also, we observed that the individuals belonging to the same household do not have the same target variable. We solved this by taking the head of household as reference and gave all the household mem-bers the same target variable


---
Feature Engineering:
------
Feature engineering is a very important in getting better predictions. Existing features cannot always explain which all households are poor. Also, linear combination of features cannot explain the nonline-ar relationship between features. Subject Expertise plays a very important role in feature Engineering. After studying and understanding the features we came up with some new features that we think are crucial for the prediction of cover type.
First, the easiest step: we have removed all the squared variables. Sometimes variables are squared or transformed as part of feature engineering because it can help linear models learn relationships that are non-linear. However, since we will be using more complex models, these squared features are redun-dant. They are highly correlated with the non-squared version, and hence can hurt our model by adding irrelevant information and slowing down training.
For most of the household level variables, we can simply keep them as is, Since, we want to make pre-dictions for each household, we use these variables as features. However, we can also remove some re-dundant variables and add in some more features derived from existing data.
Variables r4t3, tamhog, tamviv, hhsize and hogar_total, are all highly correlated with one another. In fact, hhsize has a perfect correlation with tamhog and hogar_total. We will remove these two variables because the information is redundant. We can also remove r4t3 because it has a near perfect correlation with hhsize. tamviv is not necessarily the same as hhsize because there might be family members that are not living in the household. 

---
Feature Construction
------
Two new features were constructed at house hold level. We have created one feature that counts the luxuries in the house like refrigerator, tablet, television, mobile. So, we have added all these variables and constructed a feature called “positive”. The other one counts the missing of the necessities in the house like toilet, electricity, floor, ceiling and water. We have added these variables again to create a new feature and named it “negative”.


---
Modeling:
------
**Multinomial Logistic Regression:**

Multinomial logistic regression is often defined as a simple extension of binary logistic regression that allows for more than two categories of the dependent or outcome variable. Like binary logistic regres-sion, multinomial logistic regression uses maximum likelihood estimation to derive the probability cat-egorical membership and requires diagnostic methods based on residuals analysis to evaluate the model adequacy. The model does not perform well with our data, its accuracy is only 0.695.


**Support Vector Machine:** 

Support Vector Machine is a supervised machine learning algorithm used for both classification or re-gression challenges. Here we plot each data item as a point in n-dimensional space, n being the number of features we have with the value of each feature being the value of a coordinate. Then, we perform classification by finding the hyper-plane that differentiate the two classes. Support Vectors are simply the co-ordinates of individual observation. Support Vector Machine is a frontier which best segregates the two classes. When the number of dimensions in the data increases, we use kernels which project the lower dimensional data in to a higher dimension where we can separate the data easily using hyper-planes. 
Our data set is not too large, so it will be not being taking much time for us to run SVM, which usually takes very high time for huge data sets. With support vector machine we achieved 62% accuracy. Even SVM, which generally performs well with most of the datasets, did not perform well with our data.

**Random Forest:** 

Random forests are an ensemble method which is based on decision trees. It is implemented by several decision trees during training time and outputting the mode or mean prediction of the trees. Random forest can reduce the overfitting problem of decision trees at a large level. Random Forest can lower variance without much of an increase in bias. We used expand.grid to find Optimal value (with respect to Out-of-Bag error estimate) for Number of variables available for splitting at each tree node which is called mtry and number of trees to grow which is called ntree. From the Elevation clearly shows up as the most significant variable both in terms of mean decrease in Accuracy as well as contributing to re-duction in node impurity. The variable importance plot indicates the importance of the predictors in the model. From this plot we see that the meaneduc, dependency, overcrowding, hogar_nin, explains max-imum variance in the data and the accuracy for our model is 0.9148 for the validation dataset.

**Gradient Boosting Method:** 

Gradient Boosting Method is a machine learning technique for classification problems, which produces a prediction model in the form of an ensemble of weak prediction models, typically decision trees. It builds the model in a stage-wise fashion like other boosting methods do, and it generalizes them by al-lowing optimization of an arbitrary differentiable loss function. The purpose of boosting is to sequen-tially apply the weak classification algorithm to repeatedly modified versions of the data, thereby pro-ducing a sequence of weak classifiers. It inherits all the good features of 10 trees (Variable selection, mixed predictors, missing data) and improves on the weak features such as prediction performance. 
Variable Importance plot (Fig 3.1) indicates the importance of the predictors in the model. From this plot we see that the r4m3, hhsize, lugar5, dis, rez_esc, v18q1, v14a explains maximum variance in the data and the accuracy for our model is 0.9264 for the validation dataset.

**K-Nearest Neighbors:**
The KNN approach simply predicts a new sample using the K-closest samples from the training set. Unlike other methods in this chapter, KNN cannot be cleanly summarized by a model, Instead, its construction is solely based on the individual samples from the training data. To predict a new sample for regression, KNN identifies that sample’s KNNs in the predictor space. The predicted response for the new sample is then the mean of the K neighbors’ responses..Because the KNN method fundamentally depends on distance between samples, the scale of the predictors can have a dramatic influence on the distances among samples.For our model for K= 1, gives us an accuracy of 0.824.Which is less than our GBM model.

------
Choosing the Final Model
---
In each of the models the final model selected is based up on the accuracy and kappa values from the resamples and the parameter tuning. The final model from multinomial Logistic Regression, Gradient Boosting, Support Vector Machine, K-Nearest Neighbor, Random Forest, Neural Network is selected by checking the accuracy and kappa values of the model and also finding the accuracy, kappa and ROC on the validation set.
It is observed that both accuracy and kappa values are high for Gradient Boosting. Also, let us check the performance of these models on the validation set. 

---
List of external links that I used for help
--
1.	Kaggle - CostaRica Poverty Level Prediction. Retrieved from Kaggle:https://www.kaggle.com/c/costa-rican-household-poverty-prediction
2.	Kuhn, M., & Kjell J.(2013). Applied Predictive Modeling. New York: Spring
3.	Giovanni Romeo, Mariangela Sciandra, Marcello Chiodi(2012). How to de-fine deviance residuals in Multinomial Regression.
4.	Natekin, A., & Knoll, A. (2013). Gradient boosting machines, a tutori-al.Frontiers in neurorobotics
5.	Zygmunt, Z. (2013, 01 14). Feature selection in practice. Retrieved from FastML: http://fastml.com/feature-selection-in-practice/



***   Thankyou for checking ,Hope this help you in your work!***

