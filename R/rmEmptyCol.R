#' Remove Empty columns from data.frame or matrix
#'
#' @author Aaron Hayman
#' @description This function takes a data.frame or matrix and looks for columns containing no data.
#' This is judged by all elements in a column containing only NAs, NaNs or empty strings, which maybe user defined.
#' Columns can be removed when containing a mixture of things judged to contain no data.
#' Currently lists are not supported. haven library reads .sas7bdat files and returns them as lists. Such would need to be converted to data.frames.
#'
#' @param d a data.frame or matrix
#' @param na.strings a character vector of strings that should be considered as NA. Regular expressions are not supported.
#' Strings are matched exactly: "." is not the same as ". "
#'
#' @return A data.frame or matrix as was supplied, with any empty columns removed.
#'
#' @examples
#' d = data.frame(a=1:100000,b=NA,c=runif(100000),d='.',e=sample(c('.',NA,'NA',NaN,''),100000,TRUE))
#' e = rmEmptyCol(d)
#' @export
rmEmptyCol = function(d,na.strings = ".")
{
  if(class(d)=='data.frame')
  {
    classes = sapply(d,class)
    keep = KeepCols(d,classes,na.strings)
    if(length(keep)<ncol(d)) stop('data.frame has column of unexpected class')


  }else
  if(class(d)=='matrix')
  {
    if(mode(d)=='character')
    {
      keep=KeepColsCM(d,na.strings)
    }else if(mode(d)=='numeric' || mode(d)=='logical')
    {
      keep=KeepColsNM(d)
    }else{
      stop(paste("no method for matrix of mode",mode(d)))
    }
  }else{
    stop(paste("no method for objects of class",class(d)))
  }

  return(d[,keep])
}






