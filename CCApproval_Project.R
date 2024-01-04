# Credit Card’s Fate: A Statistical Analysis on the Factors that Influence Client's Credit Card Approval# 

# Load packages 
library(ggpubr) # allows us to create and customize results ggplot2-based graphs for our report 
library(tidyverse) # loads ggplot2 for data visualization. dplyr for data manipulation
library(dplyr) 
library(broom) # tidy model output 
library("Hmisc") # used to find correlation and p-values
library(caret) # helps us split our data into training and testing sets
library(pscl) #helps us get pseudo R-square for logistic regression 

### Step 1 Import Data ###
credit_df <- read.csv("Desktop/clean_dataset.csv")

# Drop columns not relevant to helping us determine credit card approval 
credit_df= subset(credit_df, select= -c(ZipCode))
# Zip Code column is dropped because it is not relevant to helping us determine credit card approval 

### Step 2- Descriptive Statistics and Data Visualization ### 

# change categorical variables to factors
credit_df$Ethnicity <- as.factor(credit_df$Ethnicity)
credit_df$Industry <-as.factor(credit_df$Industry)
credit_df$Citizen <- as.factor(credit_df$Citizen)

summary(credit_df) # summary statistics of our data 
# Insights: The mean age of credit card applicants in the data set is 31.51, mean debt is 4.76 
# (in hundreds), mean income is $1017.4, mean credit score is 2.4 (in hundreds).

# use cross tabulation for variables with binary outcome
xtabs(~Approved + PriorDefault, data= credit_df)
xtabs(~Approved + Gender, data= credit_df)
xtabs(~Approved + Married, data= credit_df)
xtabs(~Approved + BankCustomer, data= credit_df)
xtabs(~Approved + Employed, data= credit_df)
xtabs(~Approved + DriversLicense, data= credit_df)
xtabs(~Approved, data= credit_df)
############Creating Data Visualizations ##############

approved <- table(credit_df$Approved)  # visualizing our target variable
barplot (approved)
# The bar chart of approvals show us that more than half of the applicants in our data were denied credit

# Stacked Bar Chart for Approval Based on Ethnicity 
data1 <- table(credit_df$Approved, credit_df$Ethnicity)
data1

data_1 <- as.data.frame(data1)
names(data_1) <-c ("ApprovalStatus","Ethnicity","Frequency")
data_1

EthnicityPlot<- ggplot(data_1,aes(x=Ethnicity,y=Frequency, fill=ApprovalStatus))+
  geom_bar(position= "dodge", stat="identity")
EthnicityPlot
print(EthnicityPlot + ggtitle("Credit Card Approval By Ethnicity"))
# 0 = not approved, 1= approved                      
# Graph interpretation: The approval among the different ethnicity groups are pretty even; 
# it doesn't seem like one ethnicity group is being favored over others in terms of credit card 
# approval decision. This tells us that Ethnicity may not be a significant independent variable for 
# our model, but further analysis and test need to be run to confirm this assumption. 

# Stacked Bar Chart for Approval based on Prior Default
data2<- table( credit_df$Approved, credit_df$PriorDefault)
data2

data_2<- as.data.frame(data2)
names(data_2)<- c("ApprovalStatus","PriorDefault","Frequency")
data_2
#  361 applicants have prior default and 329 don't have prior default 
DefaultPlot <- ggplot(data_2, aes(x=PriorDefault, y= Frequency,fill=ApprovalStatus))+
  geom_bar(stat="identity") +
  ggtitle("Credit Card Approval By PriorDefault")
DefaultPlot
# 0= no prior default, 1= prior default
# Graph interpretation: We can see that there is a obvious difference in credit card approval for 
# applicants with a prior default and applicants without. When we look at percentage of approval 
# it is 23/329 = 7% compared to 284/361 = 78.67%. This tells us that Prior Default may have significant
# impact on whether or not an applicant gets approved for credit, with those who have no prior default
# placed at a higher chance of approval.

# Stacked Bar Chart for Approval Based on Credit Score 
data3 <-table(credit_df$Approved, credit_df$CreditScore)
data3

data_3 <- as.data.frame(data3)
names(data_3) <- c("ApprovalStatus","CreditScore","Frequency")
data_3

CreditPlot <- ggplot(data_3, aes(x=CreditScore, y= Frequency,fill=ApprovalStatus))+
  geom_bar(stat="identity")
CreditPlot
print(CreditPlot + ggtitle("Credit Card Approval By CreditScore"))
# Graph Interpretation: It can be seen that applicants with lower credit score tend to get rejected more 
# often than those who have higher credit scores. Thus, Credit Score may be another significant predictor. 

# Stacked Bar Chart for Approval based on Employment Status 
data4 <-table(credit_df$Approved, credit_df$Employed)
data4

data_4 <- as.data.frame(data4)
names(data_4) <- c("ApprovalStatus","Employed","Frequency")
data_4

EmployedPlot <- ggplot(data_4, aes(x=Employed, y= Frequency,fill=ApprovalStatus))+
  geom_bar(stat="identity")
EmployedPlot
print(EmployedPlot + ggtitle("Credit Card Approval By Employment Status"))
# 0 = not employed, 1= employed
# Graph Interpretation: It can be seen that applicants who are employed are more likely to get 
# approved than those who are not employed

### Step 3: Feature engineering: Data pre-processing ###
# Creating unique identifiers for categorical independent variables 

# Unique identifiers for Ethnicity
credit_df$Ethnicity <-ifelse(credit_df$Ethnicity == 'Asian', 1, 0)+
                       ifelse(credit_df$Ethnicity == 'White', 2, 0)+
                      ifelse(credit_df$Ethnicity == 'Black', 3, 0)+
                      ifelse(credit_df$Ethnicity == 'Latino', 4, 0)
 # dummy variable for Ethnicity 'Other' is not needed because we only need (k-1) dummy variables
credit_df$Ethnicity # outputs transformed column 

# Unique identifiers for Industry
credit_df$Industry<- ifelse(credit_df$Industry=='Industrials', 1,0)+
                     ifelse(credit_df$Industry=='Materials', 2,0)+
                     ifelse(credit_df$Industry=='CommunicationServices', 3,0)+
                      ifelse(credit_df$Industry=='Transport', 4,0)+
                      ifelse(credit_df$Industry=='InformationTechnology', 5,0)+
                      ifelse(credit_df$Industry=='Financials', 6,0)+
                      ifelse(credit_df$Industry=='Energy', 7,0)+
                      ifelse(credit_df$Industry=='Real Estate', 8,0)+
                      ifelse(credit_df$Industry=='Utilities', 9,0)+
                      ifelse(credit_df$Industry=='ConsumerDiscretionary', 10,0)+
                      ifelse(credit_df$Industry=='Education', 11,0)+
                      ifelse(credit_df$Industry=='ConsumerStaples', 12,0)+
                      ifelse(credit_df$Industry=='Healthcare', 13,0)
      # dummy variable for Health Industry is not needed because we only need (k-1)dummy variables,
      # where k = # of levels 
      # we will know if a client is in the Health Industry if they output '0'
credit_df$Industry # outputs transformed column 

# Unique identifiers for Citizen
credit_df$Citizen <- ifelse(credit_df$Citizen=='ByBirth', 1,0)+
                     ifelse(credit_df$Citizen=='ByOtherMeans', 2,0)
credit_df$Citizen
# we will know if a client is a temporary citizen if their output is '0'

### Step 4: Feature Selection - Selecting contributing variables to our model ###
credit_cor= credit_df
corr <- rcorr(as.matrix(credit_cor))
round(corr$r, digits= 2) # outputs correlation matrix, focusing on coefficients rounded to 2 decimal places
# The independent variables " Bank Customer" and "Married" appears to be correlated with each other 
# since r= 0.99 which is greater than 0.7. We will drop one of the columns to avoid errors.

credit_df2 <- subset(credit_df, select= -c(Married))

### Step 5:  Model Building ####

#Splitting the data into training and testing sets #
set.seed(3456)
trainIndex <- createDataPartition(credit_df2$Approved, p=.70, list= FALSE, times= 1 )
Train <- credit_df[ trainIndex,]
Test <- credit_df[-trainIndex,]

# First model with all the variables 
# the "glm" function enables us to call the logistic regression model
model <-glm(Approved~Gender+Age+Debt+BankCustomer+
              PriorDefault+YearsEmployed+Employed+CreditScore+DriversLicense+ Income+
              Ethnicity + Citizen+ Industry, data= Train, family= binomial)
summary(model)
# Result: Looking at the p-values for our independent variables, any variable that has a p-value greater 
# than 0.05 is NOT significant
# The only significant variables are: Prior Default, Credit Score, Income, Citizen, and Industry.
# All other variables will be dropped to improve our model 

# Remove insignificant variables and rebuild the model 
model2 <-glm(Approved~PriorDefault+CreditScore+ Income+
              Citizen+ Industry, data= Train, family= binomial)
summary(model2)

model2$coefficients # focus on coefficients to form estimated logistic regression equation 
# b0= -1.06 , b1= 3.59, b2= 0.18 , b3= 0.0005, b4= -1.37, b5= -0.09

# Find odds ratio to better interpret coefficients 
exp(cbind(OR=coef(model2),confint(model2)))
# the value 1 is not included in the confidence intervals of the odds ratio, which means they are all
# significant
### Result Interpretation #### 
# Prior Default - having a prior default (1) vs. not having a prior default (0), changes the log odds of 
  # credit card approval by 3.59
# Credit Score- for every one unit change in credit score, the log odds of credit approval
  # increases by 0.18
# Income- for every one unit change in income, the log odds of credit approval increases by 0.0005
# Citizen- being a citizen "by other means" (2) vs. being a citizen "by birth" (1) changes the log odds
  # of credit approval by -1.37
# Industry- working in the "Materials" industry (2) versus working in the " Industrial" industry (1)
  # changes the log odds of credit card approval by -0.09.

### Step 6- Evaluating model performance ###

# McFadden's R- square
pR2(model2) 
# Our McFadden R-square is 0.52, which indiates good model fit

# Predicting on the test data the probability of being approved for credit
fitted.results <- predict(model2, Test,type='response')
head(fitted.results)

# showing the probability of applicants getting approved or not 
probability <- data.frame(prob=as.data.frame(fitted.results), actual= Test$Approved)
head(probability)

#Creating a threshold for our probability values- using default of 0.5
fitted.results <- ifelse(fitted.results >0.5,1,0)
head(fitted.results)
# applicants with p above 0.5 are considered approved (1)

### Assessing model accuracy ####
misClasificError <- mean(fitted.results != Test$Approved)
print(paste('Accuracy', 1-misClasificError))
# Our model's prediction accuracy is 86%, which is good. The misclassification error rate is 14%.

# count the number of approved vs not approved applicants in the test data 
counts1 <- table(Test$Approved)
counts1

# using confusion matrix
table(Test$Approved, fitted.results)
# Interpretation: The 2x2 table shows the predicted values (columns) from the model vs. the 
# actual values from the test data set.
# The predicted number of approvals was 98 and the actual value was 94.The predicted number of denials
# is 109 vs. 113 actual denials. The predicted and actual values are very close, indicating that 
# our model is relatively good at predicting whether an individual will be approved for credit or not.

