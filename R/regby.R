#' @title A regby function that summarizes a stratified analysis results of
#' regression models 
#' @description This function helps to extract the important
#' regression summaries such as coefficients, confidence intervals, and P-value
#' in stratified analysis. Currently, this package is implemented for the most
#' commonly used regression analysis such as linear regression, logistic
#' regression, Poisson regression, proportional odds logistic regression (using
#' polr in MASS package), multinomial regression (using multinom nnet package),
#' Cox proportional hazard regression (using coxph in survival package), and
#' linear mixed models (lmer in lme4 package). 
#' @param datain Is the input dataset 
#' @param byVar Is the stratifying categorical variable 
#' @param frmlYX The model formula. 
#' @param fam Distribution family for the output variable. 
#' Examples are binomial, poisson, etc. 
#' @param Pred Is a list containing the predictor 
#' variables names in the order they appear in the 
#' model formula.For example, if Z is a factor predictor variable and has a, b,
#' c, and d levels, unless otherwise the reference is re-leveled, the
#' coefficients will be output in alphabetical order with the first level being
#' the reference level. Thus, include in the "Pred" list the levels for which the
#' coefficients are output as Pred=c("Other-predictors", "Zb", "Zc", "Zd") in the
#' order they appear in the model formula. 
#' @param Factor Whether there are categorical predictors in the model. 
#' It defaults to FALSE. 
#' @param Intercept Whether you want the intercept output or not.
#'It defaults to FALSE. If you want the intercept, include Intercept 
#'as the first list Pred name lists.
#' @param EXP Whether you want the exponentiation of the estimate and CIs. EXP
#' defaults to TRUE. 
#' @param col.names Whether the user wants to rename column
#' names. Default=TRUE. 
#' @param colname lists of the column header names. 
#' @param Model The regression function name such as lm, glm, coxph. 
#' @param ...  Expandable. 
#' @return returns           
#'  \itemize{           
#'   straified regrresion table} 
#' @importFrom survival coxph 
#' @importFrom survival Surv
#' @importFrom MASS polr 
#' @importFrom nnet multinom 
#' @importFrom knitr kable 
#' @importFrom kableExtra kable_styling 
#' @importFrom lme4 lmer 
#' @importFrom lme4 VarCorr 
#' @importFrom kableExtra %>%
#' @import stats
#' @importFrom htmlTable htmlTable
#' @examples  
#' # Logistic regression analysis with a continuous and categorical predictors,
#' # and intercept not requested 
#'set.seed(123896) 
#'requireNamespace("htmlTable",quietly=TRUE)
#'x=rnorm(100) 
#'z=sample(letters[1:4], 100, TRUE) 
#' R<-c('B', 'W') 
#' cat<-sample(R, 100, TRUE) 
#' y=rbinom(100, 1, 0.5) 
#' data1<-data.frame(x=x,z=z, cat=cat, y=y ) 
#' # If Factor==TRUE include the level labels of the predictor as separate names.
#' regby(datain=data1, byVar='cat',
#' frmlYX=formula(y~x+z), 
#' fam=binomial, 
#' Model="glm", 
#' Pred=c("Intercept",
#'  "X","Zb",
#' "Zc", "Zd"),  colname=c("Strata", "Variable", "OR(95%CIs)", "P-value" ),
#' Factor=TRUE, Intercept=FALSE, EXP=TRUE)

#' # Multiple linear regression analysis with a continuous and categorical 
#' # predictors, and intercept included

#' regby(datain=data1, byVar='cat', frmlYX=formula(y~x+z), fam=guassian,
#'  Model="lm",Pred=c("Intercept", "X","Zb", "Zc", "Zd"), 
#'  colname=c("Strata", "Variable", "Beta (95%CIs)", "P-value" ), 
#'  Factor=TRUE, Intercept=TRUE, EXP=FALSE)

#' # Cox proportional hazard regression analysis with a continuous predictor

#' set.seed(1243567)
#' t<-rnorm(100, 15, 3)
#' y<-rbinom(100, 1, 0.5)
#' cat<-sample(c("M", "F"), 100, TRUE)
#' x<-rnorm(100, 5, 2)
#' z<-rpois(100,1)
#' z<-factor(z)
#' data2<-data.frame(t=t, x=x, cat=cat, y=y,z)
#' require('survival')
#' regby(datain=data2, byVar='cat', frmlYX=formula(Surv(t,y)~x),
#'  Model="coxph", Pred=c( "X"),  colname=c("Strata", "Variable", 
#'  "HR (95%CIs)", "P-value" ), Factor=TRUE, Intercept=FALSE)
#' # Proportional Odds Ordered Logistic Regression
#' x<-rnorm(50)
#' z<-sample(c(letters[1:5]), 50, TRUE)
#' cat<-sample(R, 50, TRUE)
#' y<-rbinom(50, 1, 0.5)
#' data3<-data.frame(x=x, z=z, cat=cat, y=y)

#' regby(datain=data3, byVar='cat', frmlYX=formula(z~x), Model="polr", 
#'  colname=c("Strata", "Variable", "Beta (95%CIs)", "P-value", 
#'  "Cum_Prob", "OR" ), Factor=TRUE, Intercept=FALSE, col.names = TRUE)


#' # Multinomial logistic regression
#' regby(datain=data3, byVar='cat',  frmlYX=(z~x), Model = "multinom",  
#' colname=c("Strata", "Variable", "OR (95%CIs)", "P-value" ), Factor=TRUE, 
#' Intercept=FALSE)

#' # Linear mixed effect models
#' regby(datain=data3, byVar='cat',  frmlYX=(x~y+(1|z)), Model = "lmer", 
#' col.names = FALSE)
#' @export

regby <- function(datain,
                  byVar,
                  frmlYX,
                  fam=NULL,
                  Pred,
                  Factor=FALSE,
                  Intercept=FALSE,
                  EXP=TRUE,
                  Model,
                  col.names=TRUE,
                  colname,
                  ...) {
  # Create the regression models
  if (Model=="glm") {
    REG<-by(datain, datain[,byVar], function(x) glm(frmlYX, data=x, family=fam))
  } else if (Model=="lm")
  { REG<-by(datain, datain[,byVar], function(x) lm(frmlYX, data=x))
  } else if (Model=="coxph")
  { 
    requireNamespace('survival', quietly = TRUE)
    REG<-by(datain, datain[,byVar], function(x) coxph(frmlYX, data=x))
  } else if (Model=="lmer"){
    REG<-by(datain, datain[,byVar], function(x) lmer(frmlYX, data=x))
  } else if (Model=="polr"){
    REG<-by(datain, datain[,byVar], function(x) polr(frmlYX, data=x, Hess=TRUE))
  }else if (Model== "multinom"){
    frmlYX<-noquote(deparse(substitute(frmlYX)))
    REG<-by(datain, datain[, byVar], function(x) multinom(formula(frmlYX), data=x, trace=FALSE))
  }else stop ("Your model may have not been implimented in this package yet.")
  
  # Summarize the model result
  sum1<-lapply(REG, summary)
  # get the coefficients
  ES<-lapply(sum1, coef)
  # Create a dataframe of the coefficients 
  xx<-simplify2array(ES)
  
  if (Model=="multinom" & Model!= "lmer"){
    # get the coefficients
    sum1<-lapply(REG, summary)
    ES<-lapply(sum1, coef)
    xx<-simplify2array(ES)
    ES<-xx[,2,]
    coef<-matrix(ES, ncol=1)
    estimate<-sprintf("%.2f", exp(ES))
    estimate<-matrix(estimate, ncol=1)
    
    # Get the confidence intervals
    ci<-lapply(REG, confint)
    ci<-simplify2array(ci)
    ci<-ci[2,,,]
    cie<-exp(ci)
    Lower<-sprintf("%.2f", cie[1,,])
    Upper<-sprintf("%.2f", cie[2,,])
    CI<-noquote(paste0( " (", Lower, ", ", Upper, ")"))
    
    # Get the standard errors to calculate P-values
    se<-sum1[names(sum1)]
    SE<-simplify2array(se)[row.names(simplify2array(se))=="standard.errors", ]
    SE<-simplify2array(SE)
    SE<-SE[,2,]
    SE<-matrix(SE, ncol=1)
    z<- coef/SE
    pval<-sprintf("%.4f", pnorm(abs(z), lower.tail=FALSE)*2)
  
    # Get the row names of coefficients
    Pred<-rep(row.names(xx), length(sum1))
    Strata<-sort(rep(names(REG), dim(xx)[1]))
    
    # Assemble the extrated variables
    Result<-data.frame(Strata=Strata, Variable=Pred, OR=paste0(estimate, CI), "Pval"=pval)
    
    
  }else if (Model!="polr" & Model !="multinom" & Model !="lmer") {
    
    if (length(dim(xx)==0))
    {
      if (EXP==TRUE) {
        estimate=as.vector(sprintf("%.2f", exp(xx[,1,])))
      }
      else {
        estimate=as.vector(sprintf("%.2f",xx[,1,]))
      }
      
      if (Model=="coxph"){
        P.value=as.vector(sprintf("%.4f", xx[, 5,]))
      } else
      {P.value=as.vector(sprintf("%.4f", xx[, 4,]))
      }
      
      Strata=sort(rep(names(ES), length(Pred)))
      Predictors=rep(c(Pred), length(ES))
      
      ES<-noquote(data.frame(Strata=paste0(Strata), Variable=paste0(Predictors),  estimate=paste0(estimate), P.value=paste0(P.value)))
      
      
      # Extract the 95% Confidence Intervals (CIs)
      
      CI <-lapply(REG, confint)
      
      # Let's covert the 95%CIs to exponential forms
      if (EXP==TRUE)
      { CIE<-lapply(CI, exp) }
      else {CIE<-CI }
      
      # Round to two digits
      cie<-simplify2array(CIE)
      Lower=sprintf("%.2f", cie[,1,])
      Upper=sprintf("%.2f", cie[,2,])
      Predictors=rep(Pred, length(CIE))
      Strata=sort(rep(names(CIE), length(Pred)))
      
      ci<-noquote(data.frame(Strata=paste0(Strata),
                             Variable=paste0(Predictors),
                             "95%CIs"=noquote(paste0(" (",Lower,", ", Upper, ")"))))
      
      if(Intercept==FALSE) {
        ci<-ci[ci$Variable!= "Intercept",]
        ES<-ES[ES$Variable!= "Intercept",]
      } else
      {
        ci<-ci
        ES<-ES
      }
      # Merge the point estimate, P-value, and the CIs
      Result<-merge(ES, ci, by=c("Strata", "Variable"))
      # Re-arrange the columns
      Result<-Result[,c(1,2,3, 5,4)]
      Result<-data.frame(Strata=Result$Strata, Variable=Result$Variable, "OR(95%CI)"= paste0(Result[,3], Result[,4]), "P-value"=Result$P.value)
      
    }else 
    {
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Started the if else above here.
      ES<-simplify2array(sum1)
      ES<-ES["coefficients",]
      
      requireNamespace("purrr", quietly = TRUE)
      es<-purrr::map(ES, data.frame)
      dt<-simplify2array(es)
      estimate<-dt[1,]
      
      if (EXP==TRUE) {estimate<-lapply(estimate, exp)
      }else {estimate<-estimate}
      estimate<-sprintf("%.2f", unlist(estimate))
      estimate<-noquote(estimate)
      
      pval<-dt[4,]
      pval<-sprintf("%.4f", unlist(pval))
      
      SE<-as.vector(unlist(dt[2,]))
      Lower<-as.vector(unlist(dt[1,]))-1.96*SE
      if (EXP==TRUE) {Lower<-lapply(Lower, exp)
      }else {Lower<-Lower}
      Lower<-sprintf("%.2f",Lower)
      Upper<-as.vector(unlist(dt[1,]))+1.96*SE
      if (EXP==TRUE){Upper<-lapply(Upper, exp)
      }else {Upper<-Upper}
      Upper<-sprintf("%.2f",Upper)
      Strata<-sort(rep(names(REG),dim(dt)[1]))
      #The person has to provide Pred names.
      pred<-lapply(ES, row.names)
      Pred<-matrix(unlist(pred), ncol=1)
      Strata<-rep(c(names(REG)), lapply(pred, length))
      beta<-noquote(paste0(estimate, " (", Lower, ", ", Upper, ")"))
      Intercept= grep("(Intercept)", Pred)
      Pred[Intercept]<-"Intercept"
      Result<-data.frame(Strata,Pred,  "Beta (95%CIs)"=beta, "P-value"=pval)
    }
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
  }else if (Model=="lmer"){
    
    # Linear Mixed Effect Models
    
    # Get the Estimates and 95%CIs
    sum1<-lapply(REG, summary)
    ES<-lapply(sum1, coef)
    ES<-simplify2array(ES)
    estimate<-sprintf("%.2f", ES[,1,])
    estimate<-matrix(estimate, ncol=1)
    xx<-simplify2array(sum1)
    
    sigma<-xx["varcor",]
    
    ci<-lapply(REG, confint)
    CI<-simplify2array(ci)
    
    Lower<-sprintf("%.2f", CI[3:dim(CI)[1],1,])
    
    Upper<-sprintf("%.2f", CI[3:dim(CI)[1],2,])
    beta<-paste0( estimate, " (", Lower, ", ",Upper, ")")
    beta<-noquote(matrix(beta,ncol=1))
    Strata<-sort(rep(names(REG), dim(ES)[1]))
    Beta<-data.frame(Strata=Strata, beta=beta)
    
    t<-ES[,3,]
    t<-matrix(t, ncol=1)
    pval<-sprintf("%.4f", pnorm(abs(t), lower.tail=FALSE)*2)
    Pred<-rep(row.names(CI), length(sum1))
    Intercept= grep("(Intercept)", Pred)
    Pred[Intercept]<-"Intercept"
    sig01= grep(".sig01", Pred)
    SD<-grep(".sigma", Pred)
    Pred[sig01]<-"Random Effect Intercept SD"
    Pred[SD]<-"Random Effect Residual SD"
    
    
    
    sigma<-lapply(REG, VarCorr)
    sig<-lapply(sigma, data.frame)
    sig1<-simplify2array(sig)
    sig2<-sig1["sdcor",]
    sigma1<-simplify2array(sig2)
    sigma2<-matrix(sigma1, ncol=1)
    sigma<-sprintf("%.2f",sigma2)
    
    Lower_sig<-sprintf("%.2f", CI[1:2,1,])
    
    Upper_sig<-sprintf("%.2f", CI[1:2,2,])
    sigs<-noquote(paste0(sigma, " (", Lower_sig, ", ", Upper_sig, ")"))
    sigma<-matrix(sigs, ncol=1)
    sig3<-data.frame(Strata, beta=sigma)
    Estimates<-rbind(sig3, Beta)
    Results<-Estimates[order(Estimates$Strata), ]
    Results$Pred<-Pred
    # Get the Pvalues
    pdata<-data.frame(Strata, pval)
    psig<-rep("", dim(CI)[1])
    psigdata<-data.frame(Strata, pval=psig)
    p<-rbind(pval=psigdata, pdata)
    pvals<-p[order(p$Strata),]
    
    pvals$Pred<-Pred
    Result<-merge(Results, pvals, by=c("Strata", "Pred"), sort=FALSE)
    colnames(Result)<-c("Strata","Effect Names", "Estimates (95%CIs)", "P-value")
    
    
  } else if (Model=="polr") {
    # ================================
    # Proportional Logistic Regression
    # ================================
    estimate<-noquote(matrix(sprintf("%.2f", xx[,1,]), ncol=1))
    Lower<-matrix((xx[,1,]-1.96*xx[,2,]),ncol=1, byrow=FALSE)
    Upper<-matrix((xx[,1,]+1.96*xx[,2,]), ncol=1, byrow=FALSE)
    Lower<-sprintf("%.2f", Lower)
    Upper<-sprintf("%.2f", Upper)
    CI<-noquote(paste0(" (", Lower, ", ", Upper, ")"  ))
    
    ci<-data.frame(CI)
    t.value<-matrix((xx[,3,]), ncol=1)
    pval<-noquote(sprintf("%.4f", pnorm(abs(t.value), lower.tail = FALSE)*2))
    
    Pred<-rep(row.names(xx),dim(REG))
    Strata<-sort(rep(names(REG), dim(xx)[1]))
    Result<-data.frame(Strata=Strata, Variable=Pred, "Beta_95_CI"=paste0(estimate, CI), pval=pval)
    
    func<-function(x) {
      zeta<-function(x){
        zeta<-(x)$zeta
        return(zeta)}
      zeta<-lapply(x, zeta) #The Intercepts
      xx<-simplify2array(zeta)
      Pred<-rep(row.names(xx),dim(x))
      Strata<-sort(rep(names(REG), dim(xx)[1]))
      cof<-lapply(x, coef)
      cof<-rep(cof, dim(xx)[1])
      cofs<-sort(simplify2array(cof))
      cofs<-matrix(cofs, ncol=1)
      xx<-matrix(xx, ncol=1)
      dat<-data.frame(zeta=xx, Beta=cofs)
      dat$Cum_Prob<-exp(dat$zeta-dat$Beta)/(1+exp(dat$zeta-dat$Beta))
      
      OR<-1/exp(cofs)
      dat$OR<-OR
      dat$Variable<-Pred
      dat$Strata<-Strata
      dat$Cum_Prob<-sprintf("%.2f", dat$Cum_Prob)
      dat$OR<-sprintf("%.2f", dat$OR)
      dat<-dat[, 3:6]
      return(dat)}
    
    dat<-func(REG)
    
    Result$id<-factor(1:nrow(Result))
    
    Result<-merge(x=Result, y=dat, by=c("Strata", "Variable"), all.x = TRUE)
    Result<-Result[order(Result$id),]
    
    Result<-Result[, -5]
    
  }
  
  if(col.names==TRUE){
    colnames(Result)<-colname
  }
  
  
  # Create the table
  # requireNamespace("kableExtra")
  # result <- kable(Result,format = "html", padding = 0, row.names = FALSE, full_width=FALSE)%>%kable_styling(full_width =FALSE, position="left")
  
  requireNamespace("htmlTable", quietly=TRUE)
  result<-htmlTable(Result,rnames=FALSE, css.cell=matrix("padding-left:1em", nrow=nrow   (Result)+1, ncol=ncol(Result)))
  
  # Hide the message html style printed to R console
  # sink("tmpfile")
  # Turn off warnings
  options(warn=-1)
  # Output the tables
Pred
Result
  
}

