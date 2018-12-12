#' Fast reading of NONMEM $TABLE files.
#'
#' Fast reading of NONMEM $TABLE files by utilising Rcpp package. Replaces read.table function.
#'
#' @author Aaron Hayman
#' @param p a character string containing the path of the .tab file.
#' @param headers logical. Whether headers should be included in output data frame.
#' @return A data frame containing the .tab file data.
# @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' \donttest{read_TAB(p = "...\\data.tab",headers = F)}



read_TAB = function(p,headers=TRUE){
  if(!file.exists(p)) stop(paste('could not file file at location:',p))
	d=tabB(p)
	if(class(d)=='integer')
	{
	  if(headers)
	  {
	    badcol=tabH(p)[d[2]]
	    stop(paste0('Change in data format detected in ',badcol,' at item ',d[1],'.\nCheck data file and see if FORMAT command in .ctl requires modification.'))
	  }
	  stop(paste0('Change in data format detected at item ',d[1], ' of column ',d[2],'.\nCheck data file and see if FORMAT command in .ctl requires modification.'))
	}
  class(d)='data.frame'
  attr(d,'row.names')=1:length(d[[1]])
  names(d)=paste0('V',1:ncol(d))
  if(headers)
  {
	  names(d)=tabH(p)
  }
	return(d)
}
