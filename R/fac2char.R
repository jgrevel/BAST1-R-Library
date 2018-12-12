#' Convert any vectors of factors in a data.frame or list to character vectors.
#'
#' Converts the factors in lists and data.frames to character. In lists this operates only at the top level (not whole trees in nested lists).
#' This is generally done because characters are easier to work with.
#' @author Aaron Hayman
#' @param df data.frame (or list) containing factors
#'
#' @return Returns a data.frame or list (depending on what was supplied) where all factors have been converted to characters.
#' @export
#'
#' @examples
#' d=data.frame(a=1:5,b=letters[5:1])
#'str(d)
#'str(fac2char(d))
#'
fac2char = function(df)
{
  if(!class(df)%in%c('data.frame','list'))
  {
    stop("This function is for use with lists and data.frames")
  }
  classes=sapply(df,class)
  if('list'%in%classes)
  {
    warning('This function only applies to first level in lists')
  }
  isFactor=sapply(df,is.factor)
  df[,isFactor]=sapply(df[,isFactor],as.character)
  return(df)
}
