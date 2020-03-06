#'@title Frequency table for a single variable

#' @description This is a frequency table function which prints the frequency
#'  and related row percentage for a given categorical variable.The function
#'   creates the count and related row percentages for a given
#' categorical variables and allows naming the header.
#' Provide the distinct level names as a list.
#' @param var  A categorical variable to be analyzed
#' @param row Row name for the output
#' @param colnames Level names for the categorical variable
#' @examples 
#' #Example
#' set.seed(1234)
#' letter=list(sample(letters[1:5], 20, replace=TRUE))
#' freq( letter, row=c("Letter"), colnames=c("A", "B", "C", "D", "E"))
#' @export
freq<-function(var, row, colnames){

  tab1<-table(var)
  tab=round(100*prop.table(tab1),1)
  ncols=dim(tab1)
  for (i in 1:ncols){
    tab[i]<-paste0(sprintf("%.1f", tab1[[i]]),'(',tab[[i]],"%",')')
  }
  prop<-matrix((c(dimnames(tab[1])[[1]], tab)), byrow=1, ncol=ncols, nrow=1, dimnames = NULL)
  dimnames(prop)<-list(row, colnames) #The Freq is added as row names.
  prop<-noquote(prop)
  return(prop)
}





