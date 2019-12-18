\name{readme}
\alias{readme}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
regby( 
  datain,
  byVar,
  frmlYX,
  fam  =  NULL,
  Pred,
  Factor  =  FALSE,
  Intercept  =  FALSE,
  EXP  =  TRUE,
  Model,
  col.names  =  TRUE,
  colname,
  ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datain}{Is the input dataset}
  \item{byVar}{Is the stratifying categorical variable. }

\item{ frmlYX	}{The model formula.}
\item{fam	}{Distribution family for the output variable. Examples are binomial, poisson, etc.}
\item{Pred  }{Is a list containing the predictor variables names in the order they appear in the model formula.For example, if Z is a factor predictor variable and has a, b, c, and d levels, unless otherwise the reference is re-leveled, the coefficients will be output in alphabetical order with the first level being the reference level. Thus, include in the "Pred" list the levels for which the coefficients are output as Pred = c("Other-predictors", "Zb", "Zc", "Zd") in the order they appear in the model formula.}
\item{Factor}{Whether there are categorical predictors in the model. It defaults to FALSE.}
\item{Intercept	}{Whether you want the intercept output or not. It defaults to FALSE. If you want the intercept, include Intercept as the first list Pred name lists.}

\item{EXP}{Whether you want the exponentiation of the estimate and CIs. EXP defaults to TRUE.}
\item{Model	}{The regression function name such as lm, glm, coxph.}
\item{col.names	}{Whether the user wants to rename column names. Default = TRUE.}
\item{colname	}{lists of the column header names.}
\item{...	}{Expandable.}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Daniel Tefera Dibaba, PhD 

Sr. Research Specialist

University of Tennessee

}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

EXAMPLES

Logistic regression analysis with a continuous and Categorical predictors, and intercept not requested 
 

library(StatTools)
set.seed(123896) 
requireNamespace("htmlTable",quietly = TRUE)
x = rnorm(100); 
z = sample(letters[1:4], 100, TRUE) ;
R<-c('B', 'W') ;
Cat<-sample(R, 100, TRUE) ;
y = rbinom(100, 1, 0.5) ;
data1<-data.frame(x = x,z = z, Cat = Cat, y = y ) ;

If Factor == TRUE include the level labels of the predictor as separate names.

 regby(datain = data1, byVar = 'Cat',
 frmlYX = formula(y~x+z), 
 fam = binomial, 
 Model = "glm", 
 Pred = c("Intercept",
  "X","Zb",
 "Zc", "Zd"),  colname = c("Strata", "Variable", "OR(95%CIs)", "P-value" ),
 Factor = TRUE, Intercept = FALSE, EXP = TRUE)



Multiple Linear regression analysis with a continuous and Categorical predictors, and intercept included
 
regby(datain = data1, byVar = 'Cat', frmlYX = formula(y~x+z), fam = guassian,
Model = "lm",Pred = c("Intercept", "X","Zb", "Zc", "Zd"), 
colname = c("Strata", "Variable", "Beta (95%CIs)", "P-value" ), 
Factor = TRUE, Intercept = TRUE, EXP = FALSE)


regby(datain = mtcars, byVar = 'cyl', frmlYX = formula(disp~factor(gear)+factor(am)+vs), fam = guassian,
      Model = "lm",Pred = NULL, 
      colname = c("Strata", "Variable", "Beta (95%CIs)", "P-value" ), 
      Factor = TRUE, Intercept = TRUE, EXP = FALSE)



Cox proportional hazard regression analysis with a continuous predictor

set.seed(1243567);
t<-rnorm(100, 15, 3);
y<-rbinom(100, 1, 0.5);
Cat<-sample(c("M", "F"), 100, TRUE);
x<-rnorm(100, 5, 2);
z<-rpois(100,1);
z<-factor(z);
data2<-data.frame(t = t, x = x, Cat = Cat, y = y,z);
require('survival')

regby(datain = data2, byVar = 'Cat', frmlYX = formula(Surv(t,y)~x),
Model = "coxph", Pred = c( "X"),  colname = c("Strata", "Variable", 
"HR (95%CIs)", "P-value" ), Factor = TRUE, Intercept = FALSE)

 

Proportional Odds Ordered Logistic Regression
 
 x<-rnorm(50);
 z<-sample(c(letters[1:5]), 50, TRUE);
 Cat<-sample(R, 50, TRUE);
 y<-rbinom(50, 1, 0.5);
 data3<-data.frame(x = x, z = z, Cat = Cat, y = y);

 regby(datain = data3, byVar = 'Cat', frmlYX = formula(z~x), Model = "polr", 
  colname = c("Strata", "Variable", "Beta (95%CIs)", "P-value", 
  "Cum_Prob", "OR" ), Factor = TRUE, Intercept = FALSE, col.names  =  TRUE)



Multinomial Logistic Regression
 
 regby(datain = data3, byVar = 'Cat',  frmlYX = (z~x), Model  =  "multinom",  
 colname = c("Strata", "Variable", "OR (95%CIs)", "P-value" ), Factor = TRUE, 
 Intercept = FALSE)



Linear Mixed Effect Models

 regby(datain = data3, byVar = 'Cat',  frmlYX = (x~y+(1|z)), Model  =  "lmer", 
 col.names  =  FALSE)



{
  }
}

\keyword{ StatTools, }
\keyword{regby,}
\keyword{Stratified regression}
