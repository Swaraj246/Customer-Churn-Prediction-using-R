library(car) 		  #advanced scatter plots 
library(corrplot) #	  plot correlations 
library(dplyr) 		  #data aggregates 
library(Hmisc)		  #for correlation test of multiple variables 
library(gplots)
library(psych)
library(gmodels)	  #cross tabulation
library(gplots) 	  #plot means with CI 
library(ggplot2)
set.seed(123)
options(scipen=99)
dev.off()

#Installing following package to load the excel file
install.packages("xlsx")
library(xlsx)

#Loading the file in a variable named qwe
qwe<-read.xlsx(file.choose(),2, header=TRUE)

#Viewing the dataset
View(qwe)

#Looking at the structure of dataset
str(qwe)

#Renaming the column names for better visibility
names(qwe)<-c("ID","cust_age_months","churn_rate","CHI_score_0", "CHI_score_0_1","sup_case_0", "sup_case_0_1","SP_0","SP_0_1","login_0_1","blog_articles_0_1","views_0_1","days_since_last_login")

#Here, Let's check if there are any missing values in the dataset
qwe[!complete.cases(qwe),]

#Since the number of rows returned is 0, it suggests that there are no missing values
#The structure suggests that churn rate is character so we will need it to convert it into a factor.

levels(qwe$churn_rate)

qwe$churn_rate <- as.factor(qwe$churn_rate)
#Now, replacing 1 to "yes" and 0 to "no" from the churn rate
qwe$churn_rate<-gsub("1", "yes", qwe$churn_rate)
qwe$churn_rate<-gsub("0", "no", qwe$churn_rate)


#Now changing the data types of necessary variables to numeric
qwe$cust_age_months <- as.numeric(qwe$cust_age_months)
qwe$CHI_score_0  <- as.numeric(qwe$CHI_score_0)
qwe$CHI_score_0_1  <- as.numeric(qwe$CHI_score_0_1)
qwe$sup_case_0  <- as.numeric(qwe$sup_case_0)
qwe$sup_case_0_1  <- as.numeric(qwe$sup_case_0_1)
qwe$SP_0 <- as.numeric(qwe$SP_0)
qwe$SP_0_1 <- as.numeric(qwe$SP_0_1)
qwe$login_0_1 <- as.numeric(qwe$login_0_1)
qwe$blog_articles_0_1 <- as.numeric(qwe$blog_articles_0_1)
qwe$views_0_1 <- as.numeric(qwe$views_0_1)
qwe$days_since_last_login <- as.numeric(qwe$days_since_last_login)

#Checking if there is any imbalance in the churn rate variable
count<-table(qwe$churn_rate)
count

churn <- prop.table(table(qwe$churn_rate))
churn

barplot(churn, main = "Customer Churn", col=c("steelblue"))

#Loading library ROSE to handle this imbalance by implementing oversampling and undersampling
library(ROSE)
#Balancing the data set with both over and under sampling
qwe_both1 <- ovun.sample(churn_rate~., data=train_qwe, p=0.4, seed=1, method="both")$data
table(train_qwe$churn_rate)

#Now, the minority class - YES is oversampled with replacement and majority class - NO is undersampled without replacement
#The balanced dataset has 50.4% of the NO Class and 49.5% of the YES class
View(qwe_both1)

churn1 <- prop.table(table(qwe_both1$churn_rate))
churn1

#Selecting the most important variables by using Forward Selection for this dataset to build the model.
qwe_both1$churn_rate<-as.factor(qwe_both1$churn_rate)
full<-glm(churn_rate~.,data=qwe_both1[,-c(1)], family="binomial")
full

null <- glm(churn_rate~1.,data=qwe_both1,  family="binomial")
null

#Implementing forward selection to pick important variables

step(null, scope = list(lower=null, upper=full), direction="forward")

# Let's conduct univariate analysis of the age variable 
hist(qwe$cust_age_months, main = "Customer Age", col=c("steelblue"), freq=F)
lines(density(qwe$cust_age_months), col="yellow", lwd=3)  #To show the line for numeric 
box()

summary(qwe$cust_age_months)

#Now, conducting the univariate analysis of Churn Rate
t <- table(qwe$churn_rate)
summary(qwe$churn_rate)

#Looking at the table of this variable
table(qwe$churn_rate)

# Plotting the variable to visualize the variable
barplot(t, main = "Bar Plot", xlab = "Churn Rate", ylab = "Frequency", col="steelblue")

ptab<-prop.table(t)  Check the percentage
ptab

# From the proportion table, 94.9% of the instances are NO while 5% of the instances are YES. 

# Conducting Bivariate analysis to check if there is any dependency of churn rate on customer age.
par(mfrow=c(1,2))
boxplot(cust_age_months~churn_rate, data=qwe, main="churn rate with respect to age", 
        xlab="churn rate", ylab="age(in months)",
        col=c("orange", "lightblue4"))

# From the box plot we can see that there are a lot of outliers present in the data.

chrunrate_yes<-qwe[qwe$churn_rate=="yes",]
chrunrate_no<-qwe[qwe$churn_rate=="no",]
plot(density(chrunrate_yes$cust_age_months), col="red", lwd=2.5, main="churn rate by customer age")
lines(density(chrunrate_no$cust_age_months), col="blue", lwd=2.5)
legend("topright",
       legend = c("chrun_rate=yes", "chrun_rate=no"),
       fill = c("red", "blue"))

#There are many outliers as seen from the box plot. However, it would be very hard for us to analyse anything from the box plot. So let's try to plot a histogram 

hist(qwe$cust_age_months
     [qwe$churn_rate==1],xlab="Customer Age in Months",xlim = c(0,50),
     main = 'Churn Based on Customer Age' , breaks = 30,col="darkgreen")

#Building a logistic regression model that best predicts the probability that a customer leaves
xtabs(~ cust_age_months+churn_rate, data=qwe_both1)

library(ggplot2)
options(scipen=99)

# Creating a logistic model with all the important variables found from the above data.
logit <- glm(churn_rate~CHI_score_0 + cust_age_months + days_since_last_login + 
               CHI_score_0_1 + views_0_1 + sup_case_0 + sup_case_0_1 + login_0_1 + 
               blog_articles_0_1, family = "binomial", data = qwe_both1)
# Looking at the summary of the model
summary(logit)

dev<- with(logit, null.deviance - deviance) 
dev 

# With 9 degrees of freedom
To calculate the number of predictors
dev1<-with(logit, df.null, df.residual)
dev1

# Now, finding the p-value of the model
pvalue<-with(logit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
pvalue

#The pvalue obtained here is extremely less than 0.05 we conclude that the model is significantly better than the null 9 degrees of freedom

#Predicting it on the dataset
Pred12 <- predict(logit, newdata = qwe_both1, type = "response")
Pred12
range(Pred12)

churnrate_pred <- data.frame(churn_prob=logit$fitted.values,churn_rate=qwe_both1$churn_rate,ID=qwe_both1$ID)
churnrate_pred <- churnrate_pred[order(churnrate_pred$churn_prob, decreasing=FALSE),]
churnrate_pred$rank <- 1:nrow(churnrate_pred)
churnrate_pred
# We can also represent it by plotting the data.
ggplot(data=churnrate_pred, aes(x=rank, y=churn_prob)) +
  geom_point(aes(color=churn_rate), alpha=1, shape=4, stroke=2) +
  xlab("Index") + ylab("Predicted probability of customer churn")

#The above plot shows that the probability of a customer churn not happening is lower than the customer churn ='yes' which has a higher probability.

#Prediction on data using the best threshold value from the ROC CURVE

#Library for plotting ROC curve using the training dataset
library(ROCR)

CTpred <- prediction(Pred12, qwe_both1$churn_rate)
CTperf <- performance(CTpred, "tpr", "fpr")

plot(CTperf)

#We need to find the Area under the curve value to determine if the model is good or not. Any model that is >0.5 and < than 1 is a good model

auc <- performance(CTpred, "auc")
auc <- unlist(slot(auc, "y.values"))
Auc

# Finding the best threshold value 

opt.cut <- function(CTperf){
  cut.ind <- mapply(FUN = function(x,y,p){d=(x-0)^2+(y-1)^2
  ind<- which(d==min(d))
  c(recall = y[[ind]], specificity = 1-x[[ind]],cutoff = p[[ind]])},CTperf@x.values, CTperf@y.values, CTperf@alpha.values)
}

print(opt.cut(CTperf))
#cutoff       0.4392878

# Taking cutpoint as  0.4392878, we predict the data

class <- ifelse(Pred12 >=  0.4392878, "YES", "NO")
class

#Creating a confusion Matrix
class <- as.factor(class)
table1 <- table(qwe_both1$churn_rate,class)
TN1 <- table1[1]
FN1 <- table1[2]
FP1 <- table1[3]
TP1 <- table1[4]

table1

#Accuracy
(table1[1]+table1[4])/nrow(qwe_both1)


#Recall
TP1/(TP1+FN1)


#Precision
TP1/(TP1+FP1)

cust672<- predict(logit, newdata = qwe_both1[672,], type = "response")
cust672

#0.18 is the probability that the customer will leave. 
#The threshold that is set is 0.4392. if the predicted value is more than 0.4392 it means customer churn is yes. 
#Here, 0.30<0.45 which means the prediction is NO
#The actual value of the customer_churn is "no"

#Question -The probability that a customer354 leaves
cust354<- predict(logit, newdata = qwe_both1[354,], type = "response")
Cust354

#0.4351 is the probability that the customer will leave. 
#The threshold that is set is 0.4392. if the predicted value is more than 0.4392 it means customer churn is yes. 
#Here, 0.33<0.4392 which means the prediction is NO
#The actual value of the customer_churn is "no"

#Here, we provide the list of 100 customers with the highest churn probabilities and the top three drivers of churn for each customer
View(qwe)
Pred100<- predict(logit, newdata = qwe_both1, type = "response")
Pred_order <- Pred100[order(Pred100, decreasing = T)]
final<-head(Pred_order,n=100)
final

# The input features which have the least p-value and highest coefficient from the model. These are the two metrics with which we determine the key drivers
summary(logit)

#From the coefficients, we see that CHI_score_0_1, CHI_score_0, days since last login with p_value<0.05 followed by Age.
#They serve as the key drivers to determine if the customer will churn or not.
#For every one unit increase in CHI_Score_0 and CHI_score_0_1, there is a decrease in the log of odds by 0.006 and 0.013. While for every one unit increase in days since last log in, there is an increase in the log of odds by 0.011. 
