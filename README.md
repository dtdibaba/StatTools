# StatTools
An R package that includes regby function to extract summary results of stratified regression analyses

regby {StatTools}	R Documentation
A regby function that summarizes a stratified analysis results of regression models
Description
This function helps to extract the important regression summaries such as coefficients, confidence intervals, and P-value in stratified analysis. Currently, this package is implemented for the most commonly used regression analysis such as linear regression, logistic regression, Poisson regression, proportional odds logistic regression (using polr in MASS package), multinomial regression (using multinom nnet package), Cox proportional hazard regression (using coxph in survival package), and linear mixed models (lmer in lme4 package).

Usage
regby(
  datain,
  byVar,
  frmlYX,
  fam = NULL,
  Pred,
  Factor = FALSE,
  Intercept = FALSE,
  EXP = TRUE,
  Model,
  col.names = TRUE,
  colname,
  ...
)
Arguments
datain	
Is the input dataset

byVar	
Is the stratifying categorical variable

frmlYX	
The model formula.

fam	
Distribution family for the output variable. Examples are binomial, poisson, etc.

Pred	
Is a list containing the predictor variables names in the order they appear in the model formula.For example, if Z is a factor predictor variable and has a, b, c, and d levels, unless otherwise the reference is re-leveled, the coefficients will be output in alphabetical order with the first level being the reference level. Thus, include in the "Pred" list the levels for which the coefficients are output as Pred=c("Other-predictors", "Zb", "Zc", "Zd") in the order they appear in the model formula.

Factor	
Whether there are categorical predictors in the model. It defaults to FALSE.

Intercept	
Whether you want the intercept output or not. It defaults to FALSE. If you want the intercept, include Intercept as the first list Pred name lists.

EXP	
Whether you want the exponentiation of the estimate and CIs. EXP defaults to TRUE.

Model	
The regression function name such as lm, glm, coxph.

col.names	
Whether the user wants to rename column names. Default=TRUE.

colname	
Lists of the column header names.

...	
Expandable.

Value
returns knitr table.

Examples
 
#Logistic regression analysis with a continuous and categorical predictors,
# and intercept not requested 
set.seed(123896) 
requireNamespace("htmlTable",quietly=TRUE)
x=rnorm(100) 
z=sample(letters[1:4], 100, TRUE) 
R<-c('B', 'W') 
cat<-sample(R, 100, TRUE) 
y=rbinom(100, 1, 0.5) 
data1<-data.frame(x=x,z=z, cat=cat, y=y ) 
#If Factor==TRUE include the level labels of the predictor as separate names.
regby(datain=data1, byVar='cat',
frmlYX=formula(y~x+z), 
fam=binomial, 
Model="glm", 
Pred=c("Intercept",
 "X","Zb",
"Zc", "Zd"),  colname=c("Strata", "Variable", "OR(95%CIs)", "P-value" ),
Factor=TRUE, Intercept=FALSE, EXP=TRUE)
#Multiple Linear regression analysis with a continuous and categorical 
#predictors, and intercept included
regby(datain=data1, byVar='cat', frmlYX=formula(y~x+z), fam=guassian,
 Model="lm",Pred=c("Intercept", "X","Zb", "Zc", "Zd"), 
 colname=c("Strata", "Variable", "Beta (95%CIs)", "P-value" ), 
 Factor=TRUE, Intercept=TRUE, EXP=FALSE)
#Cox proportional hazard regression analysis with a continuous predictor
set.seed(1243567)
t<-rnorm(100, 15, 3)
y<-rbinom(100, 1, 0.5)
cat<-sample(c("M", "F"), 100, TRUE)
x<-rnorm(100, 5, 2)
z<-rpois(100,1)
z<-factor(z)
data2<-data.frame(t=t, x=x, cat=cat, y=y,z)
require('survival')
regby(datain=data2, byVar='cat', frmlYX=formula(Surv(t,y)~x),
 Model="coxph", Pred=c( "X"),  colname=c("Strata", "Variable", 
 "HR (95%CIs)", "P-value" ), Factor=TRUE, Intercept=FALSE)
#Proportional Odds Ordered Logistic Regression
x<-rnorm(50)
z<-sample(c(letters[1:5]), 50, TRUE)
cat<-sample(R, 50, TRUE)
y<-rbinom(50, 1, 0.5)
data3<-data.frame(x=x, z=z, cat=cat, y=y)
regby(datain=data3, byVar='cat', frmlYX=formula(z~x), Model="polr", 
 colname=c("Strata", "Variable", "Beta (95%CIs)", "P-value", 
 "Cum_Prob", "OR" ), Factor=TRUE, Intercept=FALSE, col.names = TRUE)
#Multinomial Logistic Regression
regby(datain=data3, byVar='cat',  frmlYX=(z~x), Model = "multinom",  
colname=c("Strata", "Variable", "OR (95%CIs)", "P-value" ), Factor=TRUE, 
Intercept=FALSE)
#Linear Mixed Effect Models
regby(datain=data3, byVar='cat',  frmlYX=(x~y+(1|z)), Model = "lmer", 
col.names = FALSE)
[Package StatTools version 0.1.0 ]
