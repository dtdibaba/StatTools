# StatTools
An R package that includes regby function to extract summary results of stratified regression analyses

# Description

This function helps to extract the important regression summaries such as coefficients, confidence intervals, and P-value in stratified analysis. Currently, this package is implemented for the most commonly used regression analysis such as linear regression, logistic regression, Poisson regression, proportional odds logistic regression (using polr in MASS package), multinomial regression (using multinom nnet package), Cox proportional hazard regression (using coxph in survival package), and linear mixed models (lmer in lme4 package). For poisson regression, use the glm model with fam=poisson and EXP=TRUE to get incidence rate ratio.

# Usage 
regby( <br>
datain,<br>
byVar,<br>
frmlYX,<br>
fam = NULL,<br>
Pred,<br>
Factor = FALSE,<br>
Intercept = FALSE,<br>
EXP = TRUE,<br>
Model,<br>
col.names = TRUE,<br>
colname,<br>
…<br>
)<br>

# Arguments<br>
datain <br>
Is the input dataset

byVar <br>
Is the stratifying categorical variable. 

frmlYX <br>
The model formula.

fam <br>
Distribution family for the output variable. Examples are binomial, poisson, etc.

Pred <br>
Is a list containing the predictor variables names in the order they appear in the model formula.For example, if Z is a factor predictor variable and has a, b, c, and d levels, unless otherwise the reference is re-leveled, the coefficients will be output in alphabetical order with the first level being the reference level. Thus, include in the “Pred” list the levels for which the coefficients are output as Pred=c(“Other-predictors”, “Zb”, “Zc”, “Zd”) in the order they appear in the model formula.

Factor <br>

Whether there are categorical predictors in the model. It defaults to FALSE.

Intercept <br>

Whether you want the intercept output or not. It defaults to FALSE. If you want the intercept, include Intercept as the first list Pred name lists.

EXP <br>
Whether you want the exponentiation of the estimate and CIs. EXP defaults to TRUE.

Model <br>
The regression function name such as lm, glm, coxph.

col.names <br>
Whether the user wants to rename column names. Default=TRUE.

colname <br>
lists of the column header names.

… Expandable.
