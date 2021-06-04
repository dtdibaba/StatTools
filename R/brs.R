#'@title Summary table for bivariate regression models
#' @description This function creates a summary table from multiple
#'  bivariate regression models. If you would like to examine the bivariate
#'  associations between multiple potential confounders and an outcome variable,
#'  you may want to run multiple bivariate models and summarize 
#'  the results in one table. This function will help you with that.
#'  brs: stands for bivariate regression summary.
#' @param ...  List of the bivariate regression models separated by comma  
#' @param EXP  Logical, whether to exponentiate the estimate or not
#' @param Out  The label for the response variable (a quoted string)
#' @param EST  What to call the estimate column (a quoted string e.g. OR, HR)
#' @examples 
#' # Example
#' set.seed(1234)
#' model1<-glm(vs~gear, data=mtcars, family=binomial)
#' model2<-glm(vs~hp, data=mtcars, family=binomial)
#' model3<-glm(vs~drat, data=mtcars, family=binomial)
#' brs(model1,model2, model3,EXP=TRUE, Out="Outcome:VS", EST="OR")
#' @export

brs<-function(..., EXP=TRUE, EST, Out) {
  options(warn = -1)
  mods<-list(...)
  if(length(class(mods[[1]]))==1 & class(mods[[1]])=="list")
    mods<-lapply(mods[[1]], function(x)x)
  names(mods)<-unlist(lapply(
    match.call(expand.dots=F)$`...`,
    function(.x)deparse(.x, width.cutoff=500L)
  ))
tab<-mods
tab<-purrr::map_df(tab, broom::tidy)
options(warn = -1)
library(tidyverse)
library(gtsummary)
library(knitr)
library(kableExtra)

tab<-data.frame(tab)
tab<-tab[-agrep("Intercept", tab$term),]

if (EXP==TRUE){
tab$EST<-round(exp(tab$estimate),2) #
tab$LCI<-round(exp(tab$estimate-1.96*tab$std.error),2)
tab$UCI<-round(exp(tab$estimate+1.96*tab$std.error),2)
} else{
  tab$EST<-round(tab$estimate,2) #
  tab$LCI<-round((tab$estimate-1.96*tab$std.error),2)
  tab$UCI<-round((tab$estimate+1.96*tab$std.error),2)
}
tab$p.value<-round(tab$p.value, 4)

tab$`95% CI`<-paste0(sprintf("%.2f", tab$LCI), " to ",sprintf("%.2f",tab$UCI))
tab<-tab[, c("term", "EST","95% CI", "p.value" )]
colnames(tab)[2]<-EST
row.names(tab)<-NULL

header<-expression(header<-cat('"c(',Out, ')"',"=4"))
tab%>%kbl(caption=paste("Bivariate analysis on the association
          between predictors and ", Out) )%>%
  kable_styling(position="left", full_width=FALSE)%>%row_spec(0,color="white", background=spec_color(2))

}


